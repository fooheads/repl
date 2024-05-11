(ns fooheads.repl
  (:require
    [clojure.pprint :as pprint]
    [clojure.repl :as core-repl]
    [clojure.string :as str]
    [clojure.test]
    [clojure.tools.namespace.repl :as ctnr]
    [fooheads.miljo.a.core :as env]
    [fooheads.repl.state :as state]
    [fooheads.setish :as set]
    [fooheads.stdlib :as std]
    [lambdaisland.deep-diff2 :as ddiff]
    [pipeline.core :as pl]
    [pjstadig.humane-test-output]
    [trebl.core :as trebl]))


(env/setup-env!)
(pjstadig.humane-test-output/activate!)


(defmacro help []
  `(do
     (println "\nREPL HELP:")

     (println "\nfunctions:\n----------")
     (println
       (->>
         *ns*
         (ns-map)
         (remove
           (fn [[sym# the-var#]]
             (re-find
               #"(clojure|java)"
               (str the-var#))))
         (map first)
         (sort)
         (str/join "\n")))

     (println "\nnamespaces: \n-----------")
     (println
       (->>
         *ns*
         (ns-aliases)
         (sort-by first)
         (mapv (fn [[k# v#]] (format "%10s : %s" k# (ns-name v#))))
         (str/join "\n")))

     (println "\n\n------------------")))


(defn help-vim []
  (let [lines
        ["\nvim shortcuts"
         "---------"
         "<l>e<movement> - eval <movement>"
         "<l>eb - eval buffer"
         "gd    - go-to-definition"
         "K     - doc"
         ""
         "<l>dd - (fooheads.repl/dd) - deep-diff"
         ""
         "<l>pp - (fooheads.repl/pp) - pretty-print)"
         "<l>pt - (fooheads.repl/pt) - print-table"
         ""
         "<l>re - (ex)                - ex-data"
         "<l>rp - (pst)               - print-stack-trace"
         "<l>rr - (rr)                - local refresh/reload"
         "<l>rt - (trebl)"
         "<l>ru - (ns user)"
         ""
         "<l>tt - (clojure.test/run-tests)"
         "<l>ta - (clojure.test/run-all-tests)"
         "<l>ts - run test under cursor"
         "<l>tl - re-run last tests"
         ""
         "<l>1  - tab 1"
         "<l>2  - tab 2"
         "<l>3  - tab 3"
         "<l>4  - tab 4"]]

    (println (str/join "\n" lines))))


(defmacro playground [namn]
  (help)
  `(assert nil (str "\n----- " ~namn " -----")))


(def apropos core-repl/apropos)
(defmacro doc [name] `(core-repl/doc ~name))


(defmacro dir
  ([] `(core-repl/dir fooheads.repl))
  ([nsname] `(core-repl/dir ~nsname)))


(def find-doc core-repl/find-doc)
(defmacro source [n] `(core-repl/source ~n))
(def root-cause core-repl/root-cause)


(defn pst
  ([] (core-repl/pst *e))
  ([e] (core-repl/pst e)))


(defn ex
  ([] (ex-data *e))
  ([e] (ex-data e)))


(defn dd
  "Deep diffs two values and prints a readable and understandable output.
  When called with no args, it diffs *1 and *2. Can be called with
  expected and actual, plus an optional printer."

  ([] (dd *1 *2))

  ([expected actual] (dd expected actual (ddiff/printer)))

  ([expected actual printer]
   (ddiff/pretty-print (ddiff/diff expected actual) printer)))


(defn pp
  ([] (pp *1))
  ([v] (ddiff/pretty-print v)))


(defn pt
  ([] (pt *1))
  ([v] (pprint/print-table v)))


(def refresh! ctnr/refresh)

;;
;; trebl
;;

(defn- step-brief-view [step]
  (format "%12s %4d %-70s %-25s %50s %s"
          (:pipeline.step/state step)
          (int (or (:pipeline.step/time-spent step) 0))
          (keyword (namespace (:pipeline.step/name step)))
          (keyword (name (:pipeline.step/name step)))
          (:pipeline.step/output-path step)
          (:pipeline.step/input-paths step)))


(def ^:private trebl-options
  {:brief-viewers  ; A vector of [pred mapper] tuples. First one wins, if any.
   [[#'pl/step? #'step-brief-view]]})


(defn- sort-map-by-keys
  "Sort a map by `keys-in-order`. Any keys not in `keys-in-order` wil be placed
  last, alphabetically."
  [m keys-in-order]
  (let [unordered-keys (set/difference (set (keys m)) (set keys-in-order))
        key-index (merge
                    (zipmap keys-in-order (range))
                    (zipmap
                      (sort unordered-keys)
                      (range (count keys-in-order) 10000)))
        key-comparator (fn [k1 k2]
                         (let [i1 (get key-index k1)
                               i2 (get key-index k2)]
                           (compare i1 i2)))]
    (into (sorted-map-by key-comparator) m)))


(def ^:private pipeline-step-key-order
  [:pipeline.step/seq-id
   :pipeline.step/name
   :pipeline.step/type
   :pipeline.step/state
   :pipeline.step/time-spent
   :pipeline.step/output-schema
   :pipeline.step/validation-fns
   :pipeline.step/bindings
   :pipeline.step/function
   :pipeline.step/input-paths
   :pipeline.step/args
   :pipeline.step/output-path
   :pipeline.step/result
   :pipeline.step/failure-reason
   :pipeline.step/failure-message
   :pipeline.step/failure-value])


(defn- sort-pipeline-step-keys [pipeline]
  (update
    pipeline
    :pipeline/steps
    (fn [steps]
      (map (fn [m]
             (sort-map-by-keys m pipeline-step-key-order))
           steps))))


(def last-run pl/last-run)


(defn trebl
  ([]
   (trebl {:_last-run (sort-pipeline-step-keys (pl/last-run))
           :*e *e
           :*1 *1}))

  ([data]
   (cond
     (pl/pipeline? data)
     (trebl/trebl (sort-pipeline-step-keys data) trebl-options)

     (some? data)
     (trebl/trebl data trebl-options)

     :else
     (println "Can't trebl: no data."))))


(defn test-syms [syms]
  (reset! state/last-run-tests syms)
  (clojure.test/test-vars (map resolve syms)))


(defn run-last-tests []
  (prn "Running last tests..." @state/last-run-tests)
  (when @state/last-run-tests
    (test-syms @state/last-run-tests)))


(println "Loaded fooheads/repl.clj")

