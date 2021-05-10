(ns cryma.app.tracker
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require
    [rum.core :as rum]
    [cljs.core.async :as async :refer [<! put! >! alts!]]
    [cryma.core.api :as api]
    [cryma.core.components.browser :as browser]
    [cryma.core.components.common :as common]
    [cryma.core.components.cursors :as c]
    [cryma.core.components.html :as html]
    [clojure.string :as clostr]
    [cryma.core.components.rtext.extensions :as rte-ext]))

(defn get-event-name [{:keys [:sys/anchor :sys/entity]}]
  (let [what (:db/ident entity)
        to (get-in anchor [:sys/entity :db/ident])]
    [:tracker-event what to]))

(defn username-node-predicate [node]
  (and (= :a (:tag node))
       (->> node :content clostr/join string?)
       (->> node :content clostr/join (re-find #"^\s*\@[а-яА-ЯёЁ\w\d_-]+\s*,?\s*"))))

(defn event-title [anchor-data alt]
  (case (get-in anchor-data [:sys/entity :db/ident])
    :entity.app/rating
    (let [rating (:rating/value anchor-data)]
      (if (> rating 0) (str "+" rating) (str rating)))
    :entity.app/comment
    (let [text (html/get-text (:comment/body anchor-data) username-node-predicate)]
      (if (empty? text) alt text))
    alt))

(defn event-icon-class [{:keys [:sys/entity :rating/value]}]
  (str
    "event-icon "
    (case (:db/ident entity)
      :entity.app/rating
      (if (> value 0)
        "up" "down")
      :entity.app/comment ""
      :entity.app/sub ""
      "")))

(defn render-tracker-item [tracker item & [selected? current? opts]]
  (let [{:keys [localizer core]} tracker
        {:keys [:db/id :sys/author :sys/date :sys/anchor
                root-anchor :sys/read-by :sys/entity] :as data} @item
        browse? (#{:item-renderer.modes/browser-embedded} (:mode opts))]
    [:div
     {:class    (str
                  (if read-by "read" "unread")
                  (when-let [eid (:db/ident entity)] (str " " (name eid)))
                  (when browse? " tracker-item"))
      :on-click (fn [_]
                  (when-not (:sys/read-by @item)
                    (api/pub-event core {:qn       :event.app/tracker-event-read
                                         :event-id id
                                         :target   :server})
                    (c/swp! item assoc :sys/read-by true)))}
     (common/render-author-info tracker author)
     [:span.event
      {:title (event-title data "")}
      (localizer (get-event-name data))]
     (when-let [title (:post/title root-anchor)]
       (let [event-title (event-title anchor title)]
         (common/event-link
           tracker
           {:qn     :action.app/read-post
            :filter {:db/id (:db/id root-anchor)}}
           {:class "post-link"
            :title event-title}
           event-title)))
     (when (and browse? (:comment/body data))
       [:div.comment
        (html/html->hiccup (:comment/body data)
                           (partial rte-ext/build-post-tag tracker nil))])
     (common/date-renderer
       date
       {:component-attrs {:class (event-icon-class data)}
        :l               localizer
        :today           true
        :yesterday       true
        :just-now        100
        :since-minutes   59
        :since-hours     24
        :this-year       true
        :update-interval 120000})
     (when-not read-by [:span.status
                        [:span.control {:title (localizer :app.tracker/mark-as-read)}]])
     ]))
(rum/defcs
  tracker-event-item
  < (c/cursored)
  [state module cursor opts]
  (render-tracker-item module cursor nil nil opts))

(defn mark-read [events read-event-id]
  (mapv (fn [ev] (if (= (:db/id ev) read-event-id)
                   (assoc ev :sys/read-by true)
                   ev)) events))

(rum/defcs
  tracker-reader
  < (c/cursored nil {:nonwatch nil})
    {:will-mount
     (fn [state]
       (let [tracker (first (:rum/args state))
             cursor (second (:rum/args state))
             event-chan (:event-chan tracker)
             nonwatch (c/non-watchable state :nonwatch)
             new-chan (async/chan)]
         (c/rst! nonwatch new-chan)
         (go
           (loop []
             (when-let [[v p] (async/alts! [event-chan new-chan])]
               (when (= p event-chan)
                 (case (:qn v)
                   :event.app/tracker-event-read
                   (c/swp! cursor update :value mark-read (:event-id v)))
                 (recur))))))
       state)
     :will-unmount
     (fn [state]
       (let [nonwatch (c/non-watchable state :nonwatch)]
         (when-let [channel @nonwatch]
           (async/close! channel)))
       state)}
  [state module cursor]
  (let [{:keys [session-reader localizer]} module
        user-id (api/read-val session-reader [:principal :db/id])]
    [:div.page.tracker
     [:h1 (localizer :app/tracker)]
     (common/browser-head
       module cursor
       {:default-mode :all
        :browse-modes [{:mode-id :all}
                       {:mode-id :comments}
                       {:mode-id :ratings}
                       {:mode-id :subscribers}
                       {:mode-id   :search
                        :mode-view browser/search-widget}]})
     (browser/cursored-browser
       module cursor
       {:item-renderer       tracker-event-item
        :item-cursor-prepare (fn [item-cursor]
                               (let [item (assoc @item-cursor
                                            :root-anchor
                                            ((fn drill [anchor]
                                               (if-let [anc (:sys/anchor anchor)]
                                                 (drill anc) anchor))
                                              (:sys/anchor @item-cursor)))]
                                 (c/cur item)))
        :query-defaults      {:limit         20
                              :offset        0
                              :search-string ""
                              :target        :server}
        :query               {:qn      :action.app/read-user-tracker
                              :user-id user-id
                              :target  :server}})]))

(defn prepare-menu-source-data [tracker data]
  (let [{:keys [session-reader]} tracker]
    (vec
      (concat
        (remove #(#{:app.tracker/no-events
                    :app.tracker/read-all-events} (:menu-item/id %)) data)
        [(if (seq data)
           {:menu-item/id   :app.tracker/read-all-events
            :menu-item/type :menu-item
            :class-rule     "menu-item tracker-item read-all-events"
            :event          {:qn      :action.app/read-user-tracker
                             :user-id (api/read-val session-reader [:principal :db/id])}}
           {:menu-item/id   :app.tracker/no-events
            :class-rule     "menu-item tracker-item no-events"
            :menu-item/type :menu-item})]))))

(defn prepare-event-item
  [tracker item]
  (-> item
      (assoc :root-anchor
             ((fn drill [anchor]
                (if-let [anc (:sys/anchor anchor)] (drill anc) anchor))
               (:sys/anchor item)))
      (assoc :item-renderer (partial render-tracker-item tracker)
             :read-flag-key :sys/read-by
             :event-tags #{:event.app/tracker-event})
      (update :class-rule (fn [rule]
                            (or rule
                                (fn [item]
                                  (str
                                    (if (:sys/read-by (c/cval item))
                                      "read" "unread")
                                    " menu-item tracker-item")))))
      (update :menu-item/type (fn [type] (or type :menu-item)))
      (update :menu-item/id (fn [id] (or id (:db/id item))))))

(defn load-events [tracker]
  (let [{:keys [core session-reader]} tracker
        source (api/get-source-ref tracker)
        user-id (api/read-val session-reader [:principal :db/id])]
    (when (and
            user-id
            (not= :sys.users/guest user-id)
            (or (not (vector? @source))
                (empty? (remove #(= :app.tracker/no-events (:menu-item/id %)) @source))))
      (reset! source :loading)
      (go
        (let [response (<! (api/call-async
                             core
                             {:qn      :action.app/read-user-tracker
                              :user-id user-id
                              :limit   15
                              :offset  0
                              :target  :server}))
              items (prepare-menu-source-data
                      tracker
                      (mapv
                        (partial prepare-event-item tracker)
                        (if (api/success? core response)
                          (get-in response [:result :value])
                          [{:menu-item/id   :error
                            :class-rule     "menu-item tracker-item error"
                            :menu-item/type :error
                            :errors         (:errors response)}])))]
          (reset! source items))))
    source))

(defn handle-event-read
  [tracker event]
  (when-let [event-chan (:event-chan tracker)]
    (async/put! event-chan event))
  (case (:qn event)
    :event.app/tracker-event-read
    (let [source (api/get-source-ref tracker)
          event-id (:event-id event)]
      (swap! source mark-read event-id))))

(defn subscribe-profile-events [tracker]
  (let [{:keys [core session-reader logger notif-chan]} tracker
        user-id (api/read-val session-reader [:principal :db/id])]
    (when @notif-chan (async/close! @notif-chan))
    (when (and user-id (not= :sys.users/guest user-id))
      (go
        (let [source (api/get-source-ref tracker)
              notif-ch
              (api/call-async
                core
                {:qn      :notifier/subscribe
                 :filters [[:dtlrules
                            [:notifications.events/profile-event [:qn]]
                            [#{user-id} [:addressees]]]]})
              result (<! notif-ch)]
          (reset! notif-chan notif-ch)
          (when (api/success? core result)
            (loop []
              (when-let [event (<! notif-ch)]
                (swap! source
                       (fn [items]
                         (if (coll? items)
                           (prepare-menu-source-data
                             tracker
                             (cons (prepare-event-item tracker (:data event)) items)))))
                (recur)))
            (logger :error "Profile events subscription failed with result: " result)))))))

(defn handle-session-updated [tracker _]
  (load-events tracker)
  (subscribe-profile-events tracker))

(defrecord TrackerModule []
  api/ISpaDynamicMenuSource
  (get-source-ref [tracker]
    (:data-source tracker))
  (refresh-source [tracker]
    (load-events tracker))
  api/IModule
  api/IComponent
  (start [tracker]
    (let [{:keys [spa]} tracker
          tracker (assoc tracker
                    :data-source (atom (prepare-menu-source-data tracker []))
                    :event-chan (async/chan 1)
                    :notif-chan (atom nil))]
      (api/register-menu-source spa :tracker-menu-source tracker)
      (load-events tracker)
      (subscribe-profile-events tracker)
      tracker))
  (stop [module] module))

(defn new-tracker-module [& [m]]
  (map->TrackerModule
    (merge
      {api/id-key                       :tracker
       api/static-deps-key              [:localizer :core :uri-builder :spa :session-reader]
       api/order-servers-dependency-key [:notifier/subscribe]
       api/event-subs-key               [{:msg-filter :event.app/tracker-event-read
                                          :handler    handle-event-read}
                                         {:msg-filter :session/updated
                                          :handler    handle-session-updated}]
       api/routes-key                   [{:msg-filter      :action.app/read-user-tracker
                                          :route-component tracker-reader}
                                         ]}
      m)))