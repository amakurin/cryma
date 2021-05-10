(ns cryma.app.helpdesk
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cryma.core.components.cursors :as c]
    [cryma.core.components.channels :as ch]
    [cryma.core.components.input :as input]
    [cryma.core.components.browser :as browser]
    [rum.core :as rum]
    [cryma.core.components.common :as common]
    [cljs.core.async :as async :refer [<! put! >! alts!]]
    [cryma.core.api :as api]
    [cryma.core.tools-ui :as tools-ui]
    [cryma.core.components.rte.rte-full :as rte-full]
    [cryma.core.components.select :as select]
    [cryma.core.components.rtext.extensions :as rte-ext]
    [cryma.core.tools :as tools]
    [cryma.core.schema :as sch]
    [cryma.core.components.html :as html]
    [cryma.core.components.discussion :as disscus]))

(def menu-group-key :helpdesk/menu-group-id)

(def support-request-keys
  [:db/id
   :support-request/title
   :support-request/body
   :support-request/severity
   :support-request/category
   :sys/description])

(defn upsert-sr-request [cursor]
  (let [operation-action (if (get-in @cursor [:value :db/id])
                           :action.helpdesk/update-request
                           :action.helpdesk/create-request)]
    (-> @cursor
        (update :value
                (fn [value]
                  (-> value
                      (update :support-request/body
                              (fn [body]
                                (let [body (html/trim body)]
                                  (if (html/empty-html? body)
                                    "" body))))
                      (update :sys/description
                              (fn [descr]
                                (let [descr (html/trim descr)]
                                  (if (html/empty-html? descr)
                                    "" descr))))
                      (tools/->if (nil? (seq (:sys/description value))) dissoc :sys/description)
                      (update :support-request/category first)
                      (update :support-request/severity first)
                      (select-keys (if (= operation-action :action.helpdesk/update-request)
                                     support-request-keys
                                     (butlast support-request-keys)))
                      tools/prepare-value)))
        (assoc :qn operation-action)
        (select-keys [:qn :value])
        (assoc :target :server))))

(defn prepare-errors [errors]
  (->>
    errors
    (mapcat (fn [err]
              (if (= :value (:field err))
                (sch/flatten-map-errors [(dissoc err :field)])
                [err])))
    vec))

(defn is-my-sr? [helpdesk sr]
  (= (get-in sr [:sys/author :db/id])
     (api/read-val (:session-reader helpdesk) [:principal :db/id])))

(rum/defcs
  support-request-editor
  < (c/cursored {:local {}})
    (ch/channels)
    (c/cursored-chans
      [:rum/args 1]
      (fn [state ev]
        (let [module (first (:rum/args state))
              {:keys [core localizer]} module
              cursor (second (:rum/args state))
              ch4write (ch/ch4write state)]
          (case (:qn ev)

            :sr-editor/mount
            (if (= :action.helpdesk/update-request (:qn @cursor))
              (ch/alts!ev ch4write {:qn :sr-editor/query-value})
              (c/swp! cursor update :value (fn [v] (if v v {}))))

            :sr-editor/query-value
            (do
              (c/swp! cursor assoc :value :loading)
              (async/pipeline
                1
                ch4write
                (map (fn [response] {:qn       :sr-editor/finish-query-value
                                     :response response}))
                (api/call-async
                  core
                  (->
                    (common/event->request @cursor)
                    (dissoc :sys/guid :value)
                    (assoc
                      :qn :action.helpdesk/read-request
                      :target :server)))
                false))

            :sr-editor/authorize-edition
            (go
              (let [value (:value ev)
                    response
                    (<! (api/call-async
                          core {:qn                 :authorizer/check-permission
                                :action-id          :action.helpdesk/update-request
                                :access-category-id :access-category/helpdesk
                                :sys/author         (get-in value [:sys/author :db/id])}))]
                (if (and (api/success? core response)
                         (get-in response [:result :permitted?]))
                  (c/swp! cursor assoc :value
                          (-> value
                              (select-keys (conj support-request-keys :sys/author))
                              (update :support-request/severity vector)
                              (update :support-request/category vector)))
                  (let [errors (if (seq (:errors response))
                                 (:errors response)
                                 [{:code       :authorization/permission-denied
                                   :denied-msg {:qn :action.helpdesk/update-request}}])]
                    (c/swp! cursor assoc :value [:error errors])))))

            :sr-editor/finish-query-value
            (let [response (:response ev)]
              (if (api/success? core response)
                (ch/alts!ev ch4write {:qn    :sr-editor/authorize-edition
                                      :value (:result response)})
                (c/swp! cursor assoc :value [:error (:errors response)])))

            nil))))
    {:will-mount
     (fn [state]
       (ch/alts!stev state {:qn :sr-editor/mount})
       state)}
  [state module cursor]
  (let [{:keys [localizer core]} module
        local (c/local state :local)
        actual (c/actual cursor)
        value (:value actual)
        error? (and (vector? @value) (= :error (first @value)))
        loading? (or (= :loading @value) (nil? value) (nil? @value))
        my-sr? (is-my-sr? module value)]
    [:div
     {:class (if (= :action.helpdesk/update-request (:qn @cursor)) "page" "sr-edit")}
     (cond
       loading?
       [:div.loading (localizer :loading)]
       error?
       [:div.error (localizer (get-in @value [1 0] :error))]
       value
       [:ul.form
        {:ref "edit"}

        (common/wrap-frow
          module :support-request/title local
          (input/cursored-input
            module
            (:support-request/title value)
            {:placeholder (localizer :support-request/title)}))

        (common/wrap-frow
          module :support-request/category local
          (select/cursored-multi-select
            module
            (:support-request/category value)
            {:drop-down-item-renderer (fn [item]
                                        [:span
                                         {:class (-> item :db/ident name)}
                                         [:span.name (localizer (:db/ident item))]])
             :max-selected-count      1
             :item-renderer           (fn [item] [:span
                                                  {:title (:db/doc item)
                                                   :class (str "category "
                                                               (-> item :db/ident name))}
                                                  (localizer (:db/ident item))])
             :placeholder             (localizer :support-request/category)
             :query-template          {:qn            :helpdesk/category-search
                                       :search-string ""
                                       :order         [:db/ident]
                                       :limit         50
                                       :pattern       [:db/id :db/ident :db/doc]
                                       :target        :server}}))

        (common/wrap-frow
          module :support-request/severity local
          (select/cursored-multi-select
            module
            (:support-request/severity value)
            {:drop-down-item-renderer (fn [item]
                                        [:span
                                         {:class (-> item :db/ident name)}
                                         [:span.name (localizer (:db/ident item))]])
             :max-selected-count      1
             :item-renderer           (fn [item] [:span
                                                  {:title (:db/doc item)
                                                   :class (str "severity "
                                                               (-> item :db/ident name))}
                                                  (localizer (:db/ident item))])
             :placeholder             (localizer :support-request/severity)
             :query-template          {:qn            :helpdesk/severity-search
                                       :search-string ""
                                       :order         [:db/doc]
                                       :limit         50
                                       :pattern       [:db/id :db/ident :db/doc]
                                       :target        :server}}))

        (common/wrap-frow
          module :support-request/body local
          (rte-full/rte-full
            (:support-request/body value)
            (->
              (rte-ext/full-rte-opts module (localizer :support-request/body))
              (update :concat-tools (comp vec butlast))
              (update-in [:doc/conf :semantics] dissoc :cut))))

        (when (and (some? (:db/id @value)) (not my-sr?))
          (common/wrap-frow
            module :sys/description local
            (rte-full/rte-full
              (:sys/description value)
              (->
                (rte-ext/full-rte-opts module (localizer :sys/description))
                (update :concat-tools (comp vec butlast))
                (update-in [:doc/conf :semantics] dissoc :cut)))))

        [:li.frow.control-btns
         [:span.btn
          {:class    "cancel"
           :on-click (fn [_] (api/pub-event core {:qn :navigation/prev}))}
          (localizer :sr-editor/cancel)]
         [:span.btn
          {:class "save"
           :on-click
                  (fn [_]
                    (c/swp! local dissoc :errors)
                    (go
                      (let [response
                            (<! (api/call-async
                                  core
                                  (upsert-sr-request actual)))]
                        (if (api/success? core response)
                          (api/pub-event core {:qn :navigation/prev})
                          (c/swp! local assoc :errors (prepare-errors (:errors response)))))))}
          (localizer :sr-editor/save)]]
        (common/render-errors module (filter #(-> % :field nil?) (:errors @local)))
        ])]))

(defn support-request-item-cursor-prepare [mode item-cursor]
  (c/cur
    {:qn               :action.helpdesk/read-request
     :mode             (or mode :customer)
     :filter           {:db/id (:db/id @item-cursor)}
     :sys/cached-value @item-cursor}))

(rum/defcs
  support-request-reader
  < (c/cursored
      {:local {:permitted-action   :action.helpdesk/read-request
               :edition-permitted? false}})
    (ch/channels)
    common/mix-scroll-to
    (c/cursored-chans
      [:rum/args 1]
      (fn [state ev]
        (let [module (first (:rum/args state))
              {:keys [core]} module
              cursor (second (:rum/args state))
              ch4write (ch/ch4write state)
              mode (tools-ui/get-opts state :mode)
              browse? (= :item-renderer.modes/browser-embedded mode)
              preview? (#{:item-renderer.modes/browser-embedded
                          :item-renderer.modes/preview} mode)
              actual (c/actual cursor)
              src (if preview?
                    (:sys/cached-value actual)
                    (:value actual))
              local (c/local state :local)]
          (case (:qn ev)

            :sr-reader/mount
            (if (and (not preview?) (not browse?) (nil? (:value @cursor)))
              (ch/alts!ev ch4write {:qn :sr-reader/query-value})
              (do
                (ch/alts!ev ch4write {:qn :sr-reader/authorize-edition})
                (ch/alts!ev ch4write {:qn :sr-reader/query-subscribe})))

            :sr-reader/authorize-edition
            (go
              (let
                [response (async/<! (api/call-async
                                      core
                                      {:qn                 :authorizer/check-permission
                                       :action-id          :action.helpdesk/update-request
                                       :access-category-id :access-category/helpdesk
                                       :sys/author         (get-in @src [:sys/author :db/id])}))]
                (c/swp! local assoc :edition-permitted? (get-in response [:result :permitted?]))))

            :sr-reader/query-value
            (do
              (c/swp! cursor assoc :value :loading)
              (async/pipeline
                1
                ch4write
                (map (fn [response] {:qn       :sr-reader/finish-query-value
                                     :response response}))
                (api/call-async core (assoc (common/event->request @cursor)
                                       :qn :action.helpdesk/read-request
                                       :target :server)) false))

            :sr-reader/finish-query-value
            (let [response (:response ev)]
              (if (api/success? core response)
                (let [result (:result response)]
                  (c/swp! local
                          (fn [local]
                            (-> local
                                (update
                                  :permitted-action
                                  (fn [pa]
                                    (or (get-in result [:auth-result :permitted-action])
                                        pa)))
                                (assoc :scroll-to (:scroll-to @cursor)))))
                  (c/swp! cursor assoc :value (dissoc result :auth-result))
                  (ch/alts!ev ch4write {:qn :sr-reader/authorize-edition}))
                (c/swp! cursor assoc :value [:error (:errors response)])))
            nil))))

    {:will-mount
     (fn [state]
       (ch/alts!stev state {:qn :sr-reader/mount})
       state)}
  [state module cursor {:keys [mode]}]
  (let [{:keys [core localizer session-reader]} module
        local (c/local state :local)
        edition-permitted? (:edition-permitted? @local)
        browse? (= mode :item-renderer.modes/browser-embedded)
        preview? (#{:item-renderer.modes/browser-embedded
                    :item-renderer.modes/preview} mode)
        actual (c/actual cursor)
        src (if preview?
              (:sys/cached-value actual)
              (:value actual))
        {:keys [:db/id
                :support-request/title
                :sys/date
                :sys/author
                :sys/status
                :sys/description
                :support-request/category
                :support-request/severity
                :support-request/body] :as value} @src
        error? (and (vector? value) (= :error (first value)))
        new-status-action (when status
                            (if (= (:db/ident status) :support-request-statuses/open)
                              :support-request/do-close
                              :support-request/do-reopen))
        page-class (when (nil? (:mode @actual)) "page")]
    (cond
      (or (= :loading value) (nil? value))
      [:div {:class page-class} [:div.loading (localizer :loading)]]
      error?
      [:div {:class page-class} [:div.error (localizer (get-in value [1 0] :error))]]
      value
      [:div.profile.support-request
       {:class (str
                 page-class
                 (when preview? " preview")
                 (when browse? " browse"))}
       [:ul.form.info
        [:li.frow
         [:span.prop (str (localizer :support-request/title) ":")]
         [:span.val title]]
        [:li.frow
         [:span.prop (str (localizer :sys/date) ":")]
         [:span.val (when date
                      (tools/date>str
                        date
                        {:l           localizer
                         :format-code :date/generic}))]]
        [:li.frow
         [:span.prop (str (localizer :sys/author) ":")]
         [:span.val (common/render-author-info module author)]]
        [:li.frow
         [:span.prop (str (localizer :support-request/category) ":")]
         [:span.val (localizer (:db/ident category))]]
        [:li.frow
         [:span.prop (str (localizer :support-request/severity) ":")]
         [:span.val.severity {:class (name (:db/ident severity ""))} (localizer (:db/ident severity))]]
        [:li.frow
         [:span.prop (str (localizer :sys/status) ":")]
         [:span.val
          [:span.status {:class (when-let [st (:db/ident status)] (name st))}
           (localizer (:db/ident status))]
          (when new-status-action
            [:span.status-change
             {:class    (name (or new-status-action ""))
              :on-click (fn [_]
                          (go
                            (let [response
                                  (<! (api/call-async
                                        core
                                        {:qn     :action.helpdesk/update-request
                                         :value  {:db/id id
                                                  :sys/status
                                                         (new-status-action
                                                           {:support-request/do-close  :support-request-statuses/close
                                                            :support-request/do-reopen :support-request-statuses/open})}
                                         :target :server}))]
                              (when (api/success? core response)
                                (c/swp! src assoc :sys/status (get-in response [:result :sys/status]))))))}
             (localizer new-status-action)])]]
        [:li.frow
         [:span.prop (str (localizer :links) ":")]
         [:span.val [:ul.links
                     (when browse?
                       [:li (common/event-link
                              module {:qn     :action.helpdesk/read-request
                                      :mode   (:mode @actual)
                                      :filter {:db/id id}} {}
                              (localizer :app.helpdesk/read-link))])
                     (when edition-permitted?
                       [:li (common/event-link
                              module {:qn     :action.helpdesk/update-request
                                      :mode   (:mode @actual)
                                      :filter {:db/id id}} {}
                              (localizer :app.helpdesk/edit-link))])]]]
        (when-not browse?
          [:li.frow.sr-body
           [:span.prop (str (localizer :support-request/body) ":")]
           [:span.val (html/html->hiccup body (partial rte-ext/build-post-tag module @actual))]])
        (when (and (not browse?) (seq description))
          [:li.frow.sr-description
           [:span.prop (str (localizer :sys/description) ":")]
           [:span.val (html/html->hiccup description (partial rte-ext/build-post-tag module @actual))]])]

       [:div {:ref "discussion"}]
       (when-not preview?
         (disscus/discussion
           module
           src
           {:max-levels
            1

            :tree-query
            {:qn     :action.helpdesk/read-request-discussion
             :filter {:sys/anchor id}
             :target :server}

            :subscription-query
            {:qn      :notifier/subscribe
             :filters [[:dtlrules
                        [:notifications.events/entity-added [:qn]]
                        [:action.helpdesk/comment [[:entity :sys/entity :db/ident]]]
                        [id [[:anchor-path 0]]]]
                       [:dtlrules
                        [:notifications.events/entity-changed [:qn]]
                        [:action.helpdesk/comment [[:entity :sys/entity :db/ident]]]
                        [id [[:anchor-path 0]]]]]}
            :publish-new-query
            {:qn     :action.helpdesk/write-comment
             :value  nil
             :target :server}

            :authorize-new-query
            {:qn                 :authorizer/check-permission
             :action-id          :action.helpdesk/read-request
             :access-category-id :access-category/helpdesk
             :sys/author         (:db/id author)}

            :publish-change-query
            {:qn     :action.helpdesk/edit-comment
             :value  nil
             :target :server}

            :authorize-change-query
            {:qn                 :authorizer/check-permission
             :action-id          :action.helpdesk/read-request
             :access-category-id :access-category/helpdesk
             :sys/author         nil}

            :comment-template
            {:sys/author   (api/read-val session-reader [:principal :db/id])
             :comment/body ""
             :sys/anchor   id}

            :key-field
            :db/id

            :child-field
            :sys/_anchor

            :child-predicate-rule
            [:entity.helpdesk/comment [[:sys/entity :db/ident]]]

            :sort-field
            :sys/date}))
       ])))

(rum/defcs
  customer-dashboard
  < (c/cursored {} {:nonwatch {:notif-unsub nil}})
    (ch/channels)
    (c/cursored-chans [:rum/args 1])
  [state module cursor]
  (let [{:keys [localizer]} module
        cursor (c/actual cursor)
        browse-mode (:browse-mode @cursor :search)
        edition? (= :action.helpdesk/update-request (:qn @cursor))
        reading? (= :action.helpdesk/read-request (:qn @cursor))
        custom-action? (or edition? reading?)]
    [:div.page.helpdesk
     [:h1 (localizer :helpdesk/customer-dashboard)]
     (when-not custom-action?
       (common/browser-head module cursor
                            {:default-mode :search
                             :browse-modes [{:mode-id   :search
                                             :mode-view browser/search-widget}
                                            {:mode-id    :new-support-request
                                             :mode-class "new-sr"}]}))
     (when-not custom-action?
       (if (#{:search} browse-mode)
         (browser/cursored-browser
           module cursor
           {:item-renderer       support-request-reader
            :item-cursor-prepare (partial support-request-item-cursor-prepare :customer)
            :query-defaults      {:limit         10
                                  :offset        0
                                  :search-string ""
                                  :browse-mode   browse-mode
                                  :target        :server}
            :query               {:qn :action.helpdesk/customer-browse-requests}})
         (support-request-editor module cursor)))
     (when edition?
       (support-request-editor module cursor))
     (when reading?
       (support-request-reader module cursor {}))
     ]))

(rum/defcs
  admin-dashboard
  < (c/cursored {})
    (ch/channels)
    (c/cursored-chans [:rum/args 1])
  [state module cursor]
  (let [{:keys [localizer]} module
        cursor (c/actual cursor)
        browse-mode (:browse-mode @cursor :search)
        edition? (= :action.helpdesk/update-request (:qn @cursor))
        reading? (= :action.helpdesk/read-request (:qn @cursor))
        custom-action? (or edition? reading?)]
    [:div.page.helpdesk
     [:h1 (localizer :helpdesk/admin-dashboard)]
     (when-not custom-action?
       (common/browser-head module cursor
                            {:default-mode :search
                             :browse-modes [{:mode-id   :search
                                             :mode-view browser/search-widget}]}))
     (when-not custom-action?
       (if (#{:search} browse-mode)
         (browser/cursored-browser
           module cursor
           {:item-renderer       support-request-reader
            :item-cursor-prepare (partial support-request-item-cursor-prepare :admin)
            :query-defaults      {:limit         50
                                  :offset        0
                                  :search-string ""
                                  :browse-mode   browse-mode
                                  :target        :server}
            :query               {:qn :action.helpdesk/browse-requests}})
         (support-request-editor module cursor)))
     (when edition?
       (support-request-editor module cursor))
     (when reading?
       (support-request-reader module cursor {}))
     ]))

(defn new-menu-source [source-data refresh-source-data-fn]
  (let [source (atom source-data)]
    (reify
      api/ISpaDynamicMenuSource
      (get-source-ref [_]
        source)
      (refresh-source [helpdesk]
        (refresh-source-data-fn helpdesk source)))))

(defn register-new-menu-source [helpdesk menu-source-key source-data refresh-source-data-fn]
  (let [spa (:spa helpdesk)
        menu-source (new-menu-source source-data refresh-source-data-fn)]
    (api/register-menu-source spa menu-source-key menu-source)
    (assoc-in helpdesk [:menu-sources menu-source-key] menu-source)))

(defrecord Helpdesk []
  api/IModule
  api/IComponent
  (start [helpdesk]
    (->
      helpdesk
      (register-new-menu-source :helpdesk-customer-menu-source [] (fn [helpdesk source]))))
  (stop [helpdesk]
    helpdesk))

(defn new-helpdesk [& [m]]
  (map->Helpdesk
    (merge
      {api/id-key          :helpdesk
       api/static-deps-key [:localizer :core :spa :uri-builder :session-reader]
       api/routes-key      [{:msg-filter      :action.helpdesk/admin-dashboard
                             :route-component admin-dashboard}
                            {:msg-filter      :action.helpdesk/customer-dashboard
                             :route-component customer-dashboard}
                            {:msg-filter      {:qn :action.helpdesk/read-request :mode :admin}
                             :route-component admin-dashboard}
                            {:msg-filter      {:qn :action.helpdesk/read-request :mode :customer}
                             :route-component customer-dashboard}
                            {:msg-filter      {:qn :action.helpdesk/read-request :mode nil}
                             :route-component support-request-reader}
                            {:msg-filter       {:qn :action.helpdesk/update-request :mode :admin}
                             :route-component  admin-dashboard
                             :route-merge-data {:value :loading}}
                            {:msg-filter       {:qn :action.helpdesk/update-request :mode :customer}
                             :route-component  customer-dashboard
                             :route-merge-data {:value :loading}}
                            {:msg-filter       {:qn :action.helpdesk/update-request :mode nil}
                             :route-component  support-request-editor
                             :route-merge-data {:value :loading}}
                            ]}

      m)))
