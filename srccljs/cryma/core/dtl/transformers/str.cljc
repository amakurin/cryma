(ns cryma.core.dtl.transformers.str
  (:require
    [clojure.string :as clostr]
    #?(:cljs [goog.string.format])))

(defn do-format [format-string args]
  (apply #?(:clj  format
            :cljs goog.string.format)
         format-string
         args))

(defn parse-int [s]
  #?(:clj  (Integer/parseInt s)
     :cljs (.parseInt js/window s)))

(defn params-needed? [format-string]
  (let [rx #"(?!<%)%(?:(\d+)\$)?([-\#+\s0,(]|<)?\d*(?:\.\d+)?(?:[bBhHsScCdoxXeEfgGaAtT]|[tT][HIklMSLNpzZsQBbhAaCYyjmdeRTrDFc])"
        matches (re-seq rx format-string)]
    (loop [matches matches start 1 result #{}]
      (if (seq matches)
        (let [cur (first matches)]
          (if-let [arg-pos (second cur)]
            (let [arg-pos (parse-int arg-pos)]
              (recur (rest matches) start (conj result arg-pos)))
            (recur (rest matches) (inc start) (conj result start))))
        (mapv dec result)))))

(defn transformer
  "Transforms data to string according transformation rule (see dtl.core grammar)
  Transformer notes:
  * composition-rule = sprint-like format string (if empty string or nil used for dtlrules, then just concatenated results will be returned)
  * open-element, prefix-element, join-element, close-element = string or any val acceptable by str or using-data-preprocessor function
  * nil usings will be replaced with empty string
  * if all usings are nil then empty string will be returned"
  [transformation-context]
  (let [{:keys [rule use-data children dtlrules-options]} transformation-context]
    (if children
      (let [[composition-rule] rule
            {:keys [open ? & close]
             :or   {open "" ? "" & "" close ""}} dtlrules-options
            non-empty (remove empty? children)
            result (str open (when (seq non-empty) ?) (clostr/join & non-empty) close)]
        (if (and (string? composition-rule) (seq composition-rule))
          (do-format composition-rule [result]) result))
      (let [[composition-rule] rule]
        (if-let [params (seq (params-needed? composition-rule))]
          (if (every? nil? (map #(get use-data %) params))
            ""
            (do-format composition-rule (mapv #(if (nil? %) "" %) use-data)))
          composition-rule)))))

#_(require '[cryma.core.dtl.core :as core])

#_(time
    (core/transform
      {:filter {:db/id "kaha"} :limit 2 :offset 3}

      [:dtlrules
       ["/hubs/%s/" [[:filter :db/id]]]
       ["%s" [:dtlrules {:? "?" :& "&"}
              ["limit=%s" [:limit]]
              ["offset=%s" [:offset]]
              ["search-string=%s" [:search-string]]]]]
      transformer))

#_(time
    (dotimes [_ 1000]
      (core/transform
        {:filter {:db/id "kaha"} :limit 2 :offset 3}

        [:dtlrules
         ["/hubs/%s/" [[:filter :db/id]]]
         ["%s" [:dtlrules {:? "?" :& "&"}
                ["limit=%s" [:limit]]
                ["offset=%s" [:offset]]
                ["search-string=%s" [:search-string]]]]]
        transformer)))

#_(core/transform
    {:filter {:db/id "kaha"} :limit 2}
    ["/hubs/%s/" [[:filter :db/id]]]
    transformer)

#_(core/transform
    {:day  1 :month-k :jan
     :year 3 :hour 4}
    ["%s %s %sг в %s:%02d" [:day :month-k
                            :year :hour {:p :minute :or 0}]]
    transformer
    (fn [_ x] (if (= x :jan) "янв" x)))

#_(time
    (core/transform
      {:since-minutes 3}
      ["%d %s назад"
       [[:since-minutes]
        {:asis :minute}]]
      transformer
      (fn [_ x] (if (= x :minute) "минуты" x))))
