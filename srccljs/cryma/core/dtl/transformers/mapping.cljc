(ns cryma.core.dtl.transformers.mapping
  (:require [cryma.core.tools :as tools]))

(defmulti compose-dtlrules
          (fn [_ dtlrules-options] (:& dtlrules-options)))

(defmethod compose-dtlrules :merge-map
  [dtlrules-results {:keys [open ? close]}]
  (merge (apply merge open ? dtlrules-results) close))

(defmethod compose-dtlrules :deep-merge-map
  [dtlrules-results {:keys [open ? close] :or {open {} ? {} close {}}}]
  (tools/deep-merge (apply tools/deep-merge open ? dtlrules-results) close))

(defmethod compose-dtlrules :concat-vectors
  [dtlrules-results {:keys [open ? close]}]
  (vec (concat open (when ? [?]) dtlrules-results close)))

(defmethod compose-dtlrules :mapcat-values
  [dtlrules-results {:keys [open ? close]}]
  (vec (mapcat vals (concat open (when ? [?]) dtlrules-results close))))

(defn transformer
  "Transforms map to map according transformation rule (see dtl.core grammar)
  Transformer notes:
  * composition-rule = [keyword+] path to value of resulting map (simple keywords are allowed, will be wrapped to vector)
  * open-element, prefix-element, close-element = map or any val acceptable by operation of join-element or using-data-preprocessor function (default {})
  * join-element = ':merge-map' | ':concat-vectors' | ':deep-merge-map' | ':mapcat-values'
  - :merge-map is default and merges all results into open-element-map
  - :deep-merge-map deeply merges all results into open-element-map or {}
  - :concat-vectors concats all results and vec result
  - :mapcat-values mapcat vals all over results and vec result
  * Usings associates as vector, single using as using value."
  [transformation-context]
  (let [{:keys [rule use-data root? children dtlrules-options]} transformation-context]
    (if children
      (let [[composition-rule] rule
            dtlrules-options (->
                              dtlrules-options
                              (update :open tools/alternate-value {})
                              (update :& tools/alternate-value :merge-map))
            composition-rule (tools/ensure-vector composition-rule)
            result (compose-dtlrules
                     (keep identity children) dtlrules-options)]
        (if root? result (assoc-in {} composition-rule result)))
      (let [[composition-rule _] rule
            composition-rule (tools/ensure-vector composition-rule)
            result (if (seq (rest use-data)) use-data (first use-data))]
        (when result
          (assoc-in {} composition-rule result))))))

#_(require '[cryma.core.dtl.core :as core])

#_(time
    (dotimes [_ 1000]
      (core/transform
        {:params {:hub-id 23} :query-params {:limit 1 :offset 2 :search-string 3}}
        [:dtlrules {:open {:qn :app/browse-hub}}
         [:filter [:dtlrules
                   [:db/id [[:params :hub-id]]]
                   [:search-string [[:query-params :search-string]]]]]
         [:limit [[:query-params :limit]]]
         [:offset [[:query-params :offset]]]]
        transformer)))

#_(time
    (dotimes [_ 1000]
      (core/transform
        {:params {:hub-id 23} :query-params {:limit 1 :offset 2 :search-string 3}}
        [:dtlrules {:open {:qn :app/browse-hub}}
         [[:filter :db/id] [[:params :hub-id]]]
         [:limit [[:query-params :limit]]]
         [:offset [[:query-params :offset]]]
         [:search-string [[:query-params :search-string]]]]
        transformer)))

#_(time
    (core/transform
      {:params {:hub-id 23} :query-params {:limit 1 :offset 2 :search-string 3}}
      [:dtlrules {:open [] :& :concat-vectors}
       [[:filter :db/id] [[:params :hub-id]]]
       [:limit [[:query-params :limit]]]
       [:offset [[:query-params :offset]]]
       [:search-string [[:query-params :search-string]]]]
      transformer))

#_(time
    (core/transform
      {:params {:hub-id 23} :query-params {:limit 1 :offset 2 :search-string 3}}
      [:dtlrules {:open {:qn :app/browse-hub} :& :deep-merge-map}
       [[:filter :db/id] [[:params :hub-id]]]
       [:limit [[:query-params :limit]]]
       [:offset [[:query-params :offset]]]
       [[:filter :search-string] [[:query-params :search-string]]]]
      transformer))

#_(time
    (core/transform
      {:params {:hub-id 23} :query-params {:limit 1 :offset 2 :search-string 3}}
      [:dtlrules {:open [{:qn :app/browse-hub}] :& :mapcat-values}
       [[:filter :db/id] [[:params :hub-id]]]
       [:limit [[:query-params :limit]]]
       [:offset [[:query-params :offset]]]
       [[:filter :search-string] [[:query-params :search-string]]]]
      transformer))
