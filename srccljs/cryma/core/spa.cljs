(ns cryma.core.spa
  (:require
    [cryma.core.api :as api]
    [cryma.core.dtl.core :as dtl]
    [cryma.core.dtl.transformers.str :as str]
    [cryma.core.dtl.transformers.mapping :as mapping]
    [cryma.core.components.cursors :as c]
    [cryma.core.tools-ui :as tools-ui]
    [goog.string :as gs]
    [rum.core :as rum]
    [cryma.core.tools :as tools]))

(def state-key :spa/state)
(def menu-source-key :spa/menu-source)
(def dynamic-menu-sources-key :spa/menu-sources)
(def class-rule-key :class-rule)
(def caption-rule-key :caption-rule)
(def item-renderer-key :item-renderer)
(def current-route-rule-key :current-route-rule)

(defn do-menu-item-rule [rule item using-transformer]
  (cond
    (string? rule) rule
    (fn? rule) (do-menu-item-rule (rule item) item using-transformer)
    :else
    (dtl/transform item rule str/transformer using-transformer)))

(defn get-menu-item-class [spa item]
  (when-let [rule (class-rule-key item (class-rule-key spa))]
    (do-menu-item-rule
      rule item
      (fn [_ using-value]
        (if (keyword? using-value)
          (name using-value) (str using-value))))))

(defn get-menu-item-caption [spa item]
  (when-let [rule (caption-rule-key item (caption-rule-key spa))]
    (let [l (:l spa str)]
      (do-menu-item-rule
        rule item
        (fn [_ using-value] (l using-value))))))

(defn get-menu-source-cursor [spa menu-source]
  (if (keyword? menu-source)
    (let [sources (dynamic-menu-sources-key spa)]
      (if-let [{:keys [source source-cursor]} (and sources (menu-source @sources))]
        (do
          (when (satisfies? api/ISpaDynamicMenuSource source)
            (api/refresh-source source))
          source-cursor)
        (throw (ex-info "Unknown menu source." {:menu-source menu-source}))))
    (c/cur menu-source)))

(defn select-menu-item [spa item-id]
  (swap! (state-key spa)
         update :selected-item-id
         #(when-not (= item-id %) item-id)))

(defn unselect-menu-item [spa]
  (swap! (state-key spa) dissoc :selected-item-id))


(defmulti do-menu-item-click
          (fn [_ menu-item] (:menu-item/type menu-item)))

(defmethod
  do-menu-item-click :menu-folder
  [spa menu-item]
  (select-menu-item spa (:menu-item/id menu-item)))

(defmethod
  do-menu-item-click :menu-item
  [spa menu-item]
  (when-let [event (:event menu-item)]
    (api/pub-event (:core spa) event)))

(defn count-events [notifications item]
  (let [notification-tags (or (:notification-tags item) #{})]
    (->> notifications
         (filter (fn [item] (some notification-tags (:event-tags item))))
         count)))

(rum/defcs
  notifications-counter
  < (c/cursored)
  [state spa notification-source item-cursor]
  [:span
   (let [{:keys [notification-tags] :as item} @item-cursor]
     (when notification-tags
       (let [events-count (count-events @notification-source item)]
         (when (> events-count 0)
           [:span.events-counter events-count]))))])

(rum/defcs
  menu-item
  < (c/cursored)
  [state spa item selected-item-id current-item-id]
  (let [item-renderer (item-renderer-key @item)
        selected? (and selected-item-id @selected-item-id
                       (= (:menu-item/id @item) @selected-item-id))
        current? (and current-item-id @current-item-id
                      (= (:menu-item/id @item) @current-item-id))]
    [:li
     {:class    (str
                  (get-menu-item-class spa @item)
                  (when selected? " active")
                  (when current? " current"))
      :on-click (fn [_] (do-menu-item-click spa @item))}
     (when item-renderer
       (item-renderer item selected? current?))
     (when-not item-renderer
       [:span.menu-item-icon {:key "icon"}])
     (when-not item-renderer
       [:span.menu-item-text {:key "text"}
        (get-menu-item-caption spa @item)])
     (notifications-counter spa (:notification-source spa) item)
     ]))

(defn menu-devider? [menu-item]
  (= :menu-divider (:menu-item/type menu-item)))

(defmulti render-menu-item
          (fn [_ menu-item & _] (:menu-item/type menu-item)))

(defmethod
  render-menu-item :default
  [spa item selected-item-id current-item-id]
  (menu-item spa item selected-item-id current-item-id))

(defmethod
  render-menu-item :menu-divider
  [_ _ _ _]
  [:li.menu-divider])

(rum/defc
  submenu
  < (c/cursored)
  [spa menu-source-cursor current-item-id]
  (let [menu-source-cursor (c/actual menu-source-cursor)
        loading? (= :loading @menu-source-cursor)]
    [:div.m2
     [:span#m2-switch {:on-click #(unselect-menu-item spa)}]
     [:div.menu-divider]
     [:div.menu-inner
      (if loading?
        [:div.loading ((:l spa) :loading)]
        [:ul (map (fn [mi] (render-menu-item spa mi nil current-item-id))
                  menu-source-cursor)])]]))

(rum/defc
  menu
  < (c/cursored)
  [spa menu-source-cursor selected-item-id current-item-id]
  (let [menu-source-cursor (c/actual menu-source-cursor)
        loading? (= :loading @menu-source-cursor)]
    [:div.m1
     [:div.menu-divider]
     [:div.menu-inner
      (if loading?
        [:div.loading ((:l spa) :loading)]
        [:ul (map (fn [mi]
                    (render-menu-item spa mi selected-item-id current-item-id))
                  menu-source-cursor)])]]))

(defn get-current-item-id [spa current-route menu-source]
  (when-let [current-event (:event current-route)]
    (some
      (fn [menu-item]
        (when-let [rule (current-route-rule-key
                          menu-item (current-route-rule-key spa))]
          (when
            (and
              (some? (:event menu-item))
              (= (dtl/transform current-event rule mapping/transformer)
                 (dtl/transform (:event menu-item) rule mapping/transformer)))
            (:menu-item/id menu-item))))
      menu-source)))

(defn get-selected-item [menu-items selected-item-id]
  (when selected-item-id
    (some (fn [item]
            (when (= selected-item-id (:menu-item/id item)) item)) menu-items)))

(rum/defcs
  sidebar
  < (c/cursored {:local {:collapsed? false}})
    {:will-mount
     (fn [state]
       (let [spa (first (:rum/args state))
             root-menu-source (get-menu-source-cursor spa (menu-source-key spa))]
         (assoc state
           :root-menu-source root-menu-source)))
     :will-update
     (fn [state]
       (let [spa (first (:rum/args state))
             spa-state (second (:rum/args state))
             root-menu-source (:root-menu-source state)
             selected? (get-selected-item @root-menu-source (:selected-item-id @spa-state))
             child-menu-source (when-let [menu-source (menu-source-key selected?)]
                                 (get-menu-source-cursor spa menu-source))]
         (assoc state
           :selected? selected?
           :child-menu-source child-menu-source)))
     :transfer-state
     (fn [old new]
       (assoc new
         :root-menu-source (:root-menu-source old)
         :selected? (:selected? old)
         :child-menu-source (:child-menu-source old)))}
  [state spa spa-state]
  (let [local (c/local state :local)
        {:keys [collapsed?]} @local
        {:keys [current-route selected-item-id]} spa-state
        root-menu-source (:root-menu-source state)
        child-menu-source (:child-menu-source state)]
    [:div.sidebar
     {:class (str (when collapsed? "collapsed")
                  (when child-menu-source " dbl"))}
     [:span#m1-switch
      {:class    (when collapsed? "collapsed")
       :on-click (fn [_] (c/swp! local assoc :collapsed? (not collapsed?)))}]
     (menu spa root-menu-source selected-item-id
           (c/cur (get-current-item-id spa @current-route root-menu-source)))
     (tools-ui/wrap-css-transit
       child-menu-source submenu
       [spa child-menu-source
        (c/cur (get-current-item-id spa @current-route child-menu-source))] "submenu")]))

(rum/defc
  messagebox
  < (c/cursored)
  [spa message-box]
  (let [{:keys [text options]} @message-box]
    [:div.messagebox
     [:div.messagebox-wrapper
      [:div.closebtn
       {:on-click #(api/close-message-box spa)}]
      [:p.text text]
      [:ul.options
       (map (fn [{:keys [caption event]}]
              [:li.btn
               {:on-click (fn [_] (when event (api/pub-event (:core spa) event)))}
               caption]) options)]]]))

(rum/defc
  breadcrumbs
  [current-route]
  (let [{:keys [breadcrumbs screen]} @current-route]
    [:ul.breadcrumbs
     (->>
       (concat breadcrumbs [screen])
       (remove nil?)
       (map (fn [bc]
              [:li (if (map? bc)
                     (-> bc :action str)
                     (name bc))])))]))

(rum/defc
  logo
  [spa]
  [:a#logo
   {:href  "/"
    :title (api/get-conf spa :app-name)}
   (api/get-conf spa :logo-hiccup)])

(defn render-placeholder [spa-state placeholder-id]
  (when-let [{:keys [source-component view-component view-args]}
             (get-in @spa-state [:placeholders placeholder-id])]
    (apply view-component source-component view-args)))

(rum/defc
  main-view
  < (c/cursored)
  [spa state-cursor]
  (let [{:keys [current-route message-box] :as spa-state}
        (c/actual state-cursor)]
    [:div.heh
     [:div.header
      (logo spa)
      (breadcrumbs current-route)
      (render-placeholder spa-state :header)]
     [:div.content-wrapper
      (sidebar spa spa-state)
      (tools-ui/wrap-css-transit @message-box messagebox
                                 [spa message-box] "messagebox")
      [:div.working-area
       {:id "working-area"}
       (when-let [{:keys [route-component component]} @current-route]
         (let [event (:event current-route)]
           (rum/with-key
             (route-component component event)
             (str (:sys/guid @event)))))]]]))

(defn prepare-source [source]
  (if (satisfies? api/ISpaDynamicMenuSource source)
    (api/refresh-source source)
    source))

(defn update-notification-source
  [spa source-key source-cursor]
  (let [notification-source (:notification-source spa)
        new-notif
        (if (coll? @source-cursor)
          (->>
            @source-cursor
            (filter (fn [item]
                      (let [event-tags-key (or (:event-tags-key item) :event-tags)]
                        (event-tags-key item))))
            (filter (fn [item]
                      (let [read-flag-key (or (:read-flag-key item) :read?)]
                        (not (read-flag-key item)))))
            (mapv (fn [item]
                    (let [event-tags-key (or (:event-tags-key item) :event-tags)]
                      {:event-tags (event-tags-key item)
                       :source-key source-key}))))
          [])]
    (c/swp! notification-source
            (fn [notifs]
              (->> notifs
                   (remove (fn [notif] (= source-key (:source-key notif))))
                   (concat new-notif)
                   vec)))))

(defn attach-menu-source-watcher [spa source-key source-cursor]
  (let [src-src (c/-src source-cursor)
        path-src (c/-path source-cursor)]
    (add-watch src-src source-key
               (fn [_ _ old new]
                 (when (not= (get-in old path-src)
                             (get-in new path-src))
                   (update-notification-source spa source-key source-cursor))))))

(defrecord Spa []
  api/ISpa
  (close-message-box [spa]
    (swap! (state-key spa) dissoc :message-box))
  (show-message-box [spa message-box-info]
    (swap! (state-key spa) assoc :message-box message-box-info))
  (change-route [spa route-info]
    (swap! (state-key spa)
           (fn [state]
             (-> state
                 (assoc :current-route
                        (assoc-in route-info [:event :sys/guid] (gs/getRandomString)))
                 (dissoc :message-box))))
    (api/pub-event (:core spa)
                   {:qn    :spa/navigated
                    :event (:event route-info)}))
  (mount-component [spa placeholder-id mount-info]
    (swap! (state-key spa) assoc-in [:placeholders placeholder-id] mount-info))
  (register-menu-source [spa source-key source]
    (let [sources (dynamic-menu-sources-key spa)
          source-cursor (c/cur (prepare-source source))]
      (swap! sources assoc source-key
             {:source source :source-cursor source-cursor})
      (attach-menu-source-watcher spa source-key source-cursor)))
  (register-root-menu-group [spa group-items]
    (api/register-root-menu-group spa group-items (tools/guid)))
  (register-root-menu-group [spa group-items group-id]
    (let [menu-atom (menu-source-key spa)]
      (swap! menu-atom
             (fn [menu]
               (vec (concat
                      (remove (fn [item] (= group-id (-> item meta :group-id)))
                              menu)
                      (map (fn [item] (with-meta item {:group-id group-id}))
                           group-items)))))
      group-id))
  (unregister-root-menu-group [spa group-id]
    (let [menu-atom (menu-source-key spa)]
      (swap! menu-atom
             (fn [menu]
               (vec (remove (fn [item] (= group-id (-> item meta :group-id)))
                            menu))))))
  api/IComponent
  (start [spa]
    (let [spa-state (atom {:current-route    nil
                           :placeholders     nil
                           :selected-item-id nil
                           :root-component   nil})
          spa (-> spa
                  (assoc
                    state-key spa-state
                    dynamic-menu-sources-key (atom {})
                    :notification-source (c/cur (atom [])))
                  (update menu-source-key atom))
          root-component (rum/mount
                           (main-view spa (c/cur (state-key spa)))
                           (.getElementById js/document
                                            (api/get-conf spa :react-target-element-id)))]
      (swap! spa-state assoc :root-component root-component)
      spa))
  (stop [spa]
    spa))

(defn new-spa [& [m]]
  (map->Spa
    (merge
      {api/id-key             :spa
       api/static-deps-key    [:l :core]
       menu-source-key        []
       class-rule-key         ["%s %s" [:menu-item/type :menu-item/id]]
       caption-rule-key       ["%s" [:menu-item/id]]
       current-route-rule-key [:qn [:qn]]}
      m)))



