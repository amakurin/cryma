(ns cryma.core.components.browser
  (:require-macros
    [cljs.core.async.macros :as asyncm :refer [go go-loop]])
  (:require
    [cryma.core.components.common :as common]
    [cryma.core.api :as api]
    [cryma.core.tools-ui :as tools-ui]
    [cryma.core.tools :as tools]
    [cryma.core.components.paging :as paging]
    [cryma.core.components.input :as input]
    [cljs.core.async :refer [<! put! >! alts!]]
    [rum.core :as rum :include-macros true]
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [cryma.core.components.cursors :as c]
    [cryma.core.components.channels :as ch]))

(defn load-data [module cursor opts]
  (let [core (:core module)
        query (merge
                (:query-defaults opts)
                (common/event->request @cursor)
                (:query opts))]
    (c/swp! cursor assoc :value :loading)
    (go
      (let [response (<! (api/call-async core query))]
        (if (api/success? core response)
          (c/swp! cursor merge (:result response))
          (c/swp! cursor assoc :value [:error (:errors response)]))))))

(rum/defcs
  cursored-browser
  < (c/cursored)
  {:will-mount
   (fn [state]
     (load-data (first (:rum/args state))
                (second (:rum/args state))
                (tools-ui/get-opts state))
     state)}
  [state module cursor opts]
  (let [l (:localizer module)
        need-paging? (:need-paging? opts)
        need-paging? (if (nil? need-paging?) true need-paging?)
        item-renderer (:item-renderer opts)
        item-kf (:item-key-field opts :db/id)
        items-path (:value-items-path opts [])
        item-cursor-prepare (:item-cursor-prepare opts identity)
        actual (c/actual cursor)
        value (:value actual)
        loading? (= :loading @value)
        error? (and (vector? @value) (= :error (first @value)))
        items (get-in value items-path)]
    (assert item-renderer "item-renderer required for cursored-browser")
    [:div
     (cond
       loading?
       [:div.loading (l :loading)]
       error?
       [:div.error (l (get-in @value [1 0] :error))]
       (empty? @value)
       [:div.nothing-found (l :nothing-found)]
       :else
       [:ul.browser
        (map
          (fn [item]
            [:li.row
             {:key (item-kf @item)}
             (item-renderer
               module
               (item-cursor-prepare item)
               {:mode :item-renderer.modes/browser-embedded})])
          items)])
     (when (and need-paging? (not loading?) (not error?) @value (seq @items))
       (paging/data-pager module @actual
                          {:page-event-template
                           (:page-event-template opts
                             (common/event->request @actual))}))]))

(rum/defcs
  search-widget
  < (c/cursored)
  (ch/channels)
  (c/cursored-chans
    [:rum/args 1]
    (fn [state ev]
      (let [core (:core (first (:rum/args state)))]
        (when (= (:qn ev) :on-key-down)
        (when (= (:kk ev) :key/enter)
          (let [c @(:tgt-cursor ev)]
            (api/pub-event core
              (common/prepare-event c))))))))
  [state module cursor opts]
  (let [l (:localizer module)]
    [:div.search-wrap
     [:div.input
      (input/cursored-input
        module
        (:search-string cursor)
        {:placeholder (:search-widget/search-string-placeholder opts
                        (l :search-widget/search-string-placeholder))
         :mode        :input.modes/light})]
     (common/event-link
       module
       @cursor
       {:class "btn"
        :title (:search-widget/search-link-title opts
                 (l :search-widget/search-link-title))}
       (:search-widget/search-link-caption opts
         (l :search-widget/search-link-caption)))]))
