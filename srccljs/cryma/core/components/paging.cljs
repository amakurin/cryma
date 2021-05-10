(ns cryma.core.components.paging
  (:require
    [cryma.core.components.common :as common]
    #_[cryma.core.l10n :refer [l]]
    [rum.core :as rum :include-macros true]))

(rum/defc
  page-item
  [module {:keys [key query text class disabled? current?] :as opts}]
  [:li.pager-item
   {:key   key
    :class (str class
                (when disabled? " disabled")
                (when current? " current"))}
   (if (or current? disabled?)
     [:span text]
     (common/event-link module query {} text))])

(rum/defcs
  data-pager
  [state module data {:keys [page-event-template] :as opts}]
  (let [l (:localizer module)
        max-pages 9
        middle (quot max-pages 2)
        total (:total data)
        offset (:offset data)
        limit (:limit data)
        pg-count (.ceil js/Math (/ total limit))
        cur-pg (.ceil js/Math (/ offset limit))
        can-right (- pg-count cur-pg)
        to-show (min max-pages pg-count)
        left (- cur-pg (min cur-pg (max (inc middle) (- to-show can-right))))
        right (+ cur-pg (min can-right (max middle (- to-show cur-pg))))
        offset-query (fn [index]
                       (assoc page-event-template :offset (* limit index)))]
    [:ul.data-pager
     (when (> pg-count 2)
       (page-item module
                  {:query     (offset-query (dec cur-pg))
                   :text      (l :data-pager/prev)
                   :key       "prev"
                   :class     "prev"
                   :disabled? (<= cur-pg 0)}))
     (when (> pg-count 1)
       (map #(page-item module
                        {:query    (offset-query %)
                         :text     (str (inc %))
                         :key      %
                         :current? (= % cur-pg)})
            (range left right)))

     (when (> pg-count 2)
       (page-item module
                  {:query     (offset-query (inc cur-pg))
                   :text      (l :data-pager/next)
                   :key       "next"
                   :class     "next"
                   :disabled? (>= (inc cur-pg) pg-count)}))]))


