(ns cryma.core.dtl.core
  "Data Transformation Lang Grammar

   Grammar syntax

   | - choise
   [] - vector
   () - grouping
   {} - key-value map {k1 v1 ...}
   '' - literal
   ? - zero or one
   + - one or more

   DTL Query

   dtl-query = dtl-rules | dtl-rule
   dtl-rule = [composition-rule (usings | dtlrules)]
   dtl-rules = [':dtlrules' dtlrules-options? dtl-rule+]
   dtlrules-options = {':open'? open-element ':?'? prefix-element ':&'? join-element ':close'? closing-element ':transk'? transformer-key}
   open-element = transformer specific data to put before results
   prefix-element = transformer specific data to prefix results
   join-element = transformer specific data to join results
   close-element = transformer specific data to put after results
   transformer-key = key of transformer to use for dtl-rules. Default value is :default.
   composition-rule = transformer specific data describing composition
   usings = [(data-path | extended-data-path) +]
   extended-data-path = {':p'? data-path ':or'? use-default-value ':asis'? as-is-value ':calc'? dtl-inner-query ':custom'? custom-value}
   data-path = data-key | [data-key+]
   use-default-value = domain specific default value to use if data-path returns nil
   as-is-value = domain specific value to use as-is ignoring :p, :or and :calc
   dtl-inner-query = dtl-query (Processed without using preprocessing. If set on extendended data-path :p :or will be ignored)
   custom-value = domain specific value will be passed to using-preprocessor"
  (:require [clojure.zip :as z]))

(defn transformation-query-zipper [transformation-query]
  (z/zipper
    (fn branch? [node]
      (when-let [child-container (second node)]
        (and (vector? child-container) (= :dtlrules (first child-container)))))
    (fn children [node]
      (let [children (rest (second node))]
        (if (map? (first children))
          (vec (rest children)) (vec children))))
    (fn make-node [node children]
      (let [child-container (second node)
            dtlrules (take (if (map? (second child-container)) 2 1) child-container)]
        [(first node) (vec (concat dtlrules children))]))
    (if (= :dtlrules (first transformation-query))
      [::transformation-query-root transformation-query] transformation-query)))

(defn extend-using-path [path]
  (cond
    (and (map? path) (or (and (:p path) (vector? (:p path)))
                         (:asis path)
                         (:calc path)))
    path
    (and (map? path) (:p path))
    (update path :p vector)
    (vector? path)
    {:p path}
    :else
    {:p [path]}))

(declare -transform)

(defn using-identity [_ v] v)

(defn get-usings
  [data usings using-data-preprocessor transformers-map transk calc-using?]
  (let [using-data-preprocessor (or using-data-preprocessor using-identity)]
    (->>
      (if (vector? usings) usings (when (some? usings) [usings]))
      (map extend-using-path)
      (mapv
        (fn [{:keys [p or asis calc custom]}]
          (using-data-preprocessor
            {:transk transk :calc-using? calc-using? :custom custom}
            (cond (some? asis) asis
                  (some? calc)
                  (-transform data
                              (transformation-query-zipper calc)
                              transformers-map
                              using-data-preprocessor
                              transk
                              :calc-using)
                  :else
                  (get-in data p or))))))))

(defn get-transformer
  [transformers-map transk rule-node]
  (if-let [transformer (get transformers-map transk)]
    transformer
    (throw
      (ex-info "Transformer not found for dtl-rule"
               {:transk             transk
                :dtlrule            rule-node
                :known-transformers (vec (keys transformers-map))}))))

(defn -transform
  [data zrule transformers-map & [using-data-preprocessor transk calc-using?]]
  (if (nil? zrule)
    data
    (let [rule-node (z/node zrule)
          root? (= (first rule-node) ::transformation-query-root)
          transk (or transk :default)]
      (if (z/branch? zrule)
        (let [dtlrules-options (second (second rule-node))
              dtlrules-options (if (map? dtlrules-options) dtlrules-options {})
              inner-transk (:transk dtlrules-options transk)]
          (if (= transk inner-transk)
            ((get-transformer transformers-map transk rule-node)
              {:root?            root?
               :rule             (if root? (assoc rule-node 0 nil) rule-node)
               :dtlrules-options dtlrules-options
               :children         (loop [z (z/down zrule) res []]
                                   (let [value
                                         (-transform
                                           data z transformers-map
                                           using-data-preprocessor transk calc-using?)
                                         res (conj res value)]
                                     (if (seq (z/rights z))
                                       (recur (z/right z) res)
                                       res)))})
            (let [inner-result (-transform
                                 data
                                 (transformation-query-zipper (second rule-node))
                                 transformers-map
                                 using-data-preprocessor inner-transk calc-using?)]
              (if root?
                inner-result
                ((get-transformer transformers-map transk rule-node)
                  {:rule     rule-node
                   :use-data [inner-result]})))))
        ((get-transformer transformers-map transk rule-node)
          {:rule rule-node
           :use-data
                 (get-usings
                   data (second rule-node)
                   using-data-preprocessor
                   transformers-map transk calc-using?)})))))

(defn transform
  "Transforms data according to DTL rule using transformer-or-map and using-data-preprocessor.
  * data - kv map
  * rule - DTL rule
  * transformer-or-map - transformation fn of one argument transformation-context {:keys [rule use-data root? children dtlrules-options]} or map {k transformation-fn ...} use :default keyword for default transformer.
  * using-data-preprocessor - fn of two args: context map with keys :trunsk, :calc-using? and :custom; using value. Preprocesses using value before passing to transformer."
  [data rule transformer-or-map & [using-data-preprocessor]]
  {:pre [(or (map? transformer-or-map) (fn? transformer-or-map))]}
  (let [transformers-map (if (map? transformer-or-map)
                           transformer-or-map
                           {:default transformer-or-map})]
    (-transform
      data (transformation-query-zipper rule)
      transformers-map using-data-preprocessor)))

(defn map->dtl [m]
  (vec (cons :dtlrules (map (fn [[k v]] [v [k]]) m))))
