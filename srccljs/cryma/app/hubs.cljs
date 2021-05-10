(ns cryma.app.hubs
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cryma.core.components.cursors :as c]
    [cryma.core.components.channels :as ch]
    [cryma.core.components.input :as input]
    [cryma.core.components.browser :as browser]
    [cryma.app.posts :as posts]
    [cryma.app.anchored :as anchored]
    [rum.core :as rum]
    [cryma.core.components.common :as common]
    [cljs.core.async :as async :refer [<! put! >! alts!]]
    [cryma.core.api :as api]
    [cryma.core.tools-ui :as tools-ui]))


(rum/defcs
  hub-reader
  < (c/cursored)
  [state module cursor opts]
  (let [{:keys [localizer]} module
        mode (:mode opts)
        browse? (#{:item-renderer.modes/browser-embedded} mode)
        {:keys [:db/id :hub/description :sys/access-category] :as value} @cursor
        hub-name (:hub/name value)]
    [:div.hub
     [:ul.stat-header
      [:li.desc
       [:h1.title
        (if browse?
          (common/event-link
            module
            {:qn               :action.app/read-hub
             :filter           {:db/id id}
             :sys/cached-value value}
            {}
            [:span.hub-name hub-name])
          [:span.hub-name hub-name])
        [:span.access {:class (name (or (:db/ident access-category) ""))
                       :title (localizer (:db/ident access-category))}]]
       [:span description]]
      [:li.stats
       (anchored/subscriptions module cursor
                      {:authorize-sub-query
                       {:qn                 :authorizer/check-permission
                        :action-id          :action.app/join-hub
                        :access-category-id (:db/ident access-category)}
                       :sub-query
                       {:qn     :action.app/join-hub
                        :value  {:sys/anchor nil}
                        :target :server}})]]]))

(rum/defcs
  hub-browser
  < (c/cursored {} {:nonwatch {:notif-unsub nil}})
  (ch/channels)
  (c/cursored-chans
    [:rum/args 1]
    (fn [state ev]
      (let [module (first (:rum/args state))
            {:keys [core]} module
            nonwatch (c/actual (c/non-watchable state :nonwatch))
            cursor (second (:rum/args state))
            ch4write (ch/ch4write state)]
        (case (:qn ev)

          :hub-browser/mount
          (ch/alts!ev ch4write {:qn :hub-browser/query-value})

          :hub-browser/query-value
          (do
            (c/swp! cursor assoc :value :loading)
            (async/pipeline
              1
              ch4write
              (map (fn [response] {:qn       :hub-browser/finish-query-value
                                   :response response}))
              (api/call-async core
                              (-> @cursor
                                  (dissoc :limit :offset :search-string :browse-mode :posts)
                                  common/event->request
                                  (assoc
                                    :qn :action.app/read-hub
                                    :target :server))) false))

          :hub-browser/finish-query-value
          (let [response (:response ev)]
            (if (api/success? core response)
              (let [result (:result response)]
                (c/swp! cursor assoc :value result)
                (ch/alts!ev ch4write {:qn :hub-browser/authorize-edition})
                (ch/alts!ev ch4write {:qn :hub-browser/query-subscribe}))
              (c/swp! cursor assoc :value [:error (:errors response)])))

          :hub-browser/query-subscribe
          (when (nil? (:notif-unsub @nonwatch))
            (let [id (get-in @cursor [:value :db/id])
                  notif-ch
                  (api/call-async
                    core
                    {:qn      :notifier/subscribe
                     :filters [[:dtlrules
                                [:notifications.events/entity-changed [:qn]]
                                [:entity.app/hub [[:entity :sys/entity :db/ident]]]
                                [id [[:anchor-path 0]]]]]})]
              (go
                (let [result (<! notif-ch)
                      notif-unsub (get-in result [:result :unsub-ch])]
                  (when (api/success? core result)
                    (c/swp! nonwatch assoc :notif-unsub notif-unsub)
                    (async/pipe notif-ch ch4write false))))))

          :notifications.events/entity-changed
          (c/swp! cursor update :value merge (:entity ev))

          :sys.channels/closing
          (when-not (seq (:data-path ev))
            (when-let [unsub (:notif-unsub @nonwatch)]
              (async/close! unsub)
              (c/swp! nonwatch dissoc :notif-unsub)))
          nil))))
  {:will-mount
   (fn [state]
     (let [cursor (second (:rum/args state))]
       (c/swp!
         cursor assoc
         :posts (select-keys @cursor [:qn :limit :filter :offset :search-string :browse-mode])))
     (ch/alts!stev state {:qn :hub-browser/mount})
     state)}
  [state module cursor]
  (let [{:keys [localizer]} module
        cursor (c/actual cursor)
        browse-mode (:browse-mode @cursor)
        value (:value cursor)
        error? (and (vector? @value) (= :error (first @value)))
        loading? (or (= :loading @value) (nil? value) (nil? @value))]
    (cond
      loading?
      [:div.page [:div.loading (localizer :loading)]]
      error?
      [:div.page [:div.error (localizer (get-in @value [1 0] :error))]]
      value
      [:div.page.hub
       (hub-reader module value {:mode :item-renderer.modes/hub-browser})
       (common/browser-head module cursor
                            {:default-mode :all
                             :browse-modes [{:mode-id :all}
                                            {:mode-id :best}
                                            {:mode-id   :search
                                             :mode-view browser/search-widget}]})
       (browser/cursored-browser
         module (:posts cursor)
         {:item-renderer       posts/post-reader
          :item-cursor-prepare posts/post-item-cursor-prepare
          :query-defaults      {:limit         10
                                :offset        0
                                :search-string ""
                                :browse-mode   browse-mode
                                :target        :server}
          :query               {:qn     :action.app/browse-posts
                                :filter {:post/hub (:db/id @value)}}})
       ])))

(rum/defcs
  hubs-browser
  < (c/cursored)
  [state module cursor]
  (enable-console-print!)
  (let [{:keys [localizer]} module
        cursor (c/actual cursor)
        browse-mode (:browse-mode @cursor)]
    [:div.page
     [:h1 (localizer :app/hubs)]
     (common/browser-head module cursor
                          {:default-mode :all
                           :browse-modes [{:mode-id :all}
                                          {:mode-id :feed}
                                          {:mode-id :best}
                                          {:mode-id   :search
                                           :mode-view browser/search-widget}]})
     (browser/cursored-browser
       module cursor
       {:item-renderer  hub-reader
        :query-defaults {:limit         10
                         :offset        0
                         :search-string ""
                         :browse-mode   browse-mode
                         :target        :server}})
     ]))

(rum/defc
  hub-editor
  [module data]
  (let [{:keys [localizer]} module]
    [:div.page
     [:h2 (localizer :hubs)]
     "editor"]))

(defrecord HubsModule []
  api/IModule
  api/IComponent
  (start [module] module)
  (stop [module] module))

(defn new-hubs-module [& [m]]
  (map->HubsModule
    (merge
      {api/id-key          :hubs-module
       api/static-deps-key [:localizer :core :uri-builder]
       api/routes-key      [{:msg-filter      :action.app/browse-hubs
                             :route-component hubs-browser}
                            {:msg-filter       :action.app/read-hub
                             :route-component  hub-browser
                             :route-merge-data {:value :loading}}
                            {:msg-filter      :action.app/create-hub
                             :route-component hub-editor}
                            {:msg-filter      :action.app/edit-hub
                             :route-component hub-editor}]}
      m)))
