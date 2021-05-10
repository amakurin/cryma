(ns cryma.app.users
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require
    [cryma.core.tools-ui :as tools-ui]
    [cryma.core.tools :as tools]
    [cryma.core.components.input :as input]
    [cryma.core.components.browser :as browser]
    [rum.core :as rum]
    [cryma.core.components.common :as common]
    [cljs.core.async :as async :refer [<! put! >! alts!]]
    [cryma.core.components.cursors :as c]
    [cryma.core.components.channels :as ch]
    [cryma.core.api :as api]
    [cryma.app.posts :as posts]
    [cryma.app.anchored :as anchored]
    [cryma.core.components.rtext.extensions :as rte-ext]
    [cryma.core.components.html :as html]
    [cryma.core.components.rte.rte-full :as rte-full]))

(defn user-item-cursor-prepare [item-cursor]
  (c/cur
    {:qn               :action.app/read-user-profile
     :filter           {:db/id (:db/id @item-cursor)}
     :sys/cached-value @item-cursor}))

(rum/defcs
  comment-preview
  < (c/cursored)
  [state module cursor opts]
  (let [{:keys [localizer]} module
        cursor (c/actual cursor)
        {:keys [:sys/date :comment/body :sys/anchor]} @cursor]
    [:div.comment
     [:div.comment-head
      (common/date-renderer
        date
        {:l               localizer
         :today           true :yesterday true
         :just-now        59
         :since-minutes   59
         :since-hours     24
         :this-year       true
         :update-interval 60000})
      (common/event-link
        module
        {:qn     :action.app/read-post
         :filter {:db/id (:db/id anchor)}}
        {:class "anchored-link"}
        (:post/title anchor))]
     (html/html->hiccup body (partial rte-ext/build-post-tag module nil))]))

(defn comment-item-cursor-prepare [item-cursor]
  (let [item (update @item-cursor :sys/anchor
                     (fn drill [anchor]
                       (if-let [anchor (:sys/anchor anchor)] (drill anchor) anchor)))]
    (c/cur item)))

(defn empty-credentials []
  {:user/login-current ""
   :user/pwd-current   ""
   :user/login         ""
   :user/pwd           ""})

(rum/defcs
  credentials-change
  < (c/cursored {:opened?   false
                 :disabled? false
                 :account-cursor
                            (empty-credentials)})
  [state module]
  (let [opened? (c/local state :opened?)
        disabled? (c/local state :disabled?)
        {:keys [:user/login-current :user/login :user/pwd-current :user/pwd] :as account-cursor}
        (c/local state :account-cursor)
        {:keys [core localizer]} module]
    (if @opened?
      [:ul.form.credentials
       (common/wrap-frow
         module :user/login-current account-cursor
         (input/cursored-input
           module login-current
           {:placeholder (localizer :user/login-current)
            :id          "login-current-field"
            :disabled    @disabled?
            :maxLength   20
            :on-change   (fn [e]
                           (when (or (empty? @login)
                                     (= @login @login-current))
                             (c/rst! login (.-value (.-target e)))))}))
       (common/wrap-frow
         module :user/pwd-current account-cursor
         (input/cursored-input
           module pwd-current
           {:placeholder (localizer :user/pwd-current)
            :id          "pwd-current-field"
            :type        "password"
            :disabled    @disabled?
            :maxLength   20}))
       (common/wrap-frow
         module :user/login account-cursor
         (input/cursored-input
           module login
           {:placeholder (localizer :user/login-new)
            :id          "login-field"
            :disabled    @disabled?
            :maxLength   20}))
       (common/wrap-frow
         module :user/pwd account-cursor
         (input/cursored-input
           module pwd
           {:placeholder (localizer :user/pwd-new)
            :id          "pwd-field"
            :type        "password"
            :disabled    @disabled?
            :maxLength   20}))
       [:li.frow.btns
        [:div.btn.credentials-do-cancel
         {:on-click (fn [_]
                      (c/rst! opened? false)
                      (c/rst! disabled? false)
                      (c/rst! account-cursor (empty-credentials)))}
         (localizer :user/do-cancel)]
        [:div.btn.credentials-do-change
         {:on-click (fn [_]
                      (go
                        (c/rst! disabled? true)
                        (let
                          [response (async/<! (api/call-async
                                                core
                                                (assoc @account-cursor
                                                  :qn :app/change-credentials
                                                  :target :server)))]
                          (c/rst! disabled? false)
                          (if (api/success? core response)
                            (c/rst! opened? false)
                            (c/swp! account-cursor assoc :errors (:errors response))))))}
         (localizer :user/do-change)]]]
      [:div.btn.change-credentials
       {:on-click (fn [_]
                    (c/rst! opened? true)
                    (c/rst! disabled? false)
                    (c/rst! account-cursor (empty-credentials)))}
       (localizer :user/change-credentials)])))

(rum/defcs
  about-edit
  < (c/cursored {:opened?   false
                 :disabled? false})
  [state module user-cursor allow-edit?]
  (let [opened? (c/local state :opened?)
        disabled? (c/local state :disabled?)
        {:keys [core localizer]} module
        about (:user/about user-cursor)
        about-empty? (or (nil? @about) (html/empty-html? @about))]
    [:ul.form.about-edit
     (when allow-edit?
       [:li.frow.btns
        [:div.btn.about-edit
         {:on-click (fn [_] (c/swp! opened? not))}
         (localizer
           (if @opened?
             :user/do-cancel
             (if about-empty?
               :user/do-write :user/do-change)))]
        (when @opened?
          [:div.btn.about-save
           {:on-click (fn [_]
                        (c/swp! opened? not)
                        (go
                          (async/<!
                            (api/call-async
                              core
                              {:qn         :app/change-profile
                               :user/about @about
                               :target     :server}))))}
           (localizer :user/do-save)])])
     (when (and (not about-empty?) (not @opened?))
       [:li.frow
        (html/html->hiccup @about (partial rte-ext/build-post-tag module nil))])
     (when @opened?
       [:li.frow (rte-full/rte-full about (rte-ext/full-rte-opts module ""))])]))

(defn render-base-info [module user-cursor browse?]
  (let [{:keys [localizer session-reader]} module
        user @user-cursor
        user-id (api/read-val session-reader [:principal :db/id])
        my? (and (= user-id (:db/id user)) (not= user-id :sys.users/guest))
        about (:user/about user)
        expiry-date (:user/access-expiry user)
        days-left (tools/days-between (js/Date.) expiry-date)
        created-date (:sys/date user)]
    [:div.profile
     [:ul.form.info
      [:li.frow.registered
       [:span.prop (str (localizer :user/registered) ":")]
       [:span.val (when created-date
                    (tools/date>str
                      created-date
                      {:l           localizer
                       :format-code :date/no-time}))]]
      [:li.frow.access-level
       [:span.prop (str (localizer :user/access-level) ":")]
       [:span.val
        (common/event-link
          module
          {:qn :spa/sign-up} {}
          (localizer (get-in user [:user/access-level :db/ident])))]
       (when (and my? days-left (< days-left 0))
         [:span.val.expired (str "(" (localizer :access-expiry/expired) ")")])]
      (when (and my? (not browse?))
        [:li.frow.access-expiry
         [:span.prop (str (localizer :user/access-expiry) ":")]
         [:span.val (if expiry-date
                      (tools/date>str expiry-date
                                      {:l           localizer
                                       :format-code :date/no-time})
                      (localizer :access-expiry/never))]
         (when (and days-left (>= days-left 0))
           [:span.val.days-left (str ", " (localizer {:code      :access-expiry/days-left
                                                      :days-left days-left}))])])
      (cond
        (and (not browse?) (or about my?))
        [:li.frow.about
         [:span.prop (str (localizer :user/about-title) ":")]
         [:span.val (about-edit module user-cursor my?)]]
        (and about browse?)
        [:li.frow.about
         [:span.val
          (html/html->hiccup about (partial rte-ext/build-post-tag module nil))]])
      (when (and my? (not browse?))
        [:li.frow
         (credentials-change module)])]]))

(rum/defcs
  user-profile-reader
  < (c/cursored)
    (ch/channels)
    (c/cursored-chans
      [:rum/args 1]
      (fn [state ev]
        (let [module (first (:rum/args state))
              {:keys [core logger]} module
              browse? (= :item-renderer.modes/browser-embedded (tools-ui/get-opts state :mode))
              cursor (second (:rum/args state))
              ch4write (ch/ch4write state)]
          (case (:qn ev)

            :user-profile-reader/mount
            (when-not browse?
              (ch/alts!ev ch4write {:qn :user-profile-reader/query-value}))

            :user-profile-reader/query-value
            (do
              (c/swp! cursor assoc :value :loading)
              (async/pipeline
                1
                ch4write
                (map (fn [response] {:qn       :user-profile-reader/finish-query-value
                                     :response response}))
                (api/call-async
                  core
                  (-> @cursor
                      (dissoc :limit :offset :search-string
                              :browse-mode :posts :favs :subs :comments)
                      common/event->request
                      (assoc
                        :qn :action.app/read-user-profile
                        :target :server))) false))

            :user-profile-reader/finish-query-value
            (let [response (:response ev)]
              (if (api/success? core response)
                (let [result (:result response)]
                  (c/swp! cursor assoc :value result))
                (c/swp! cursor assoc :value [:error (:errors response)])))

            nil))))
    {:will-mount
     (fn [state]
       (let [cursor (second (:rum/args state))]
         (c/swp!
           cursor assoc
           :posts (select-keys @cursor [:qn :limit :filter :offset :search-string :browse-mode])
           :favs (select-keys @cursor [:qn :limit :filter :offset :search-string :browse-mode])
           :subs (select-keys @cursor [:qn :limit :filter :offset :search-string :browse-mode])
           :comments (select-keys @cursor [:qn :limit :filter :offset :search-string :browse-mode])))
       (ch/alts!stev state {:qn :user-profile-reader/mount})
       state)}
  [state module cursor opts]
  (let [{:keys [localizer logger]} module
        cursor (c/actual cursor)
        browse? (#{:item-renderer.modes/browser-embedded} (:mode opts))
        value (if browse? (:sys/cached-value cursor) (:value cursor))
        error? (and (vector? @value) (= :error (first @value)))
        loading? (or (= :loading @value) (nil? value) (nil? @value))
        {:keys [:db/id :sys/access-category] :as user} @value
        browse-mode (:browse-mode @cursor :profile)]
    (cond
      loading?
      [:div.page [:div.loading (localizer :loading)]]
      error?
      [:div.page [:div.error (localizer (get-in @value [1 0] :error))]]
      value
      [:div.page.user
       [:ul.stat-header
        [:li.desc
         (if browse?
           (common/render-author-info module user)
           [:h1.title [:span.user-name (:user/nickname user)]])]
        [:li.stats
         (anchored/subscriptions module value
                                 {:authorize-sub-query
                                  {:qn                 :authorizer/check-permission
                                   :action-id          :action.app/subscribe-user
                                   :access-category-id (:db/ident access-category)}
                                  :sub-query
                                  {:qn     :action.app/subscribe-user
                                   :value  {:sys/anchor nil}
                                   :target :server}})]]
       (when browse?
         (render-base-info module value browse?))

       (when-not browse?
         (common/browser-head
           module cursor
           {:default-mode :profile
            :browse-modes [{:mode-id :profile}
                           {:mode-id :posts}
                           {:mode-id :comments}
                           {:mode-id :favorities}
                           {:mode-id :subscribers}]}))
       (when-not browse?
         (case browse-mode
           :profile
           (render-base-info module value browse?)
           :posts
           (browser/cursored-browser
             module
             (:posts cursor)
             {:item-renderer       posts/post-reader
              :item-cursor-prepare posts/post-item-cursor-prepare
              :query-defaults      {:limit         10
                                    :offset        0
                                    :search-string ""
                                    :target        :server}
              :query               {:qn     :action.app/browse-posts
                                    :filter {:sys/author id}}})

           :comments
           (browser/cursored-browser
             module
             (:comments cursor)
             {:item-renderer       comment-preview
              :item-cursor-prepare comment-item-cursor-prepare
              :query-defaults      {:limit         10
                                    :offset        0
                                    :search-string ""
                                    :target        :server}
              :query               {:qn     :action.app/browse-comments
                                    :filter {:sys/author id}}})

           :favorities
           (browser/cursored-browser
             module
             (:favs cursor)
             {:item-renderer       posts/post-reader
              :item-cursor-prepare posts/post-item-cursor-prepare
              :query-defaults      {:limit         10
                                    :offset        0
                                    :search-string ""
                                    :target        :server}
              :query               {:qn                :action.app/browse-posts
                                    :browse-mode       browse-mode
                                    :filter            nil
                                    :favorities-author id}})
           :subscribers
           (browser/cursored-browser
             module
             (:subs cursor)
             {:item-renderer       user-profile-reader
              :item-cursor-prepare user-item-cursor-prepare
              :query-defaults      {:limit         10
                                    :offset        0
                                    :search-string ""
                                    :target        :server}
              :query               {:qn          :action.app/browse-user-profiles
                                    :browse-mode browse-mode
                                    :filter      nil
                                    :sub-anchor  id}})
           [:span (str browse-mode)]))
       ])))

(rum/defcs
  user-profiles-browser
  < (c/cursored)
  [state module cursor]
  (let [{:keys [localizer]} module
        actual (c/actual cursor)
        browse-mode (:browse-mode @cursor)]
    [:div.page.user-profiles
     [:h1 (localizer :app/users)]
     (common/browser-head module cursor
                          {:default-mode :all
                           :browse-modes [{:mode-id :all}
                                          {:mode-id :feed}
                                          {:mode-id :best}
                                          {:mode-id   :search
                                           :mode-view browser/search-widget}]})
     (browser/cursored-browser
       module
       actual
       {:item-renderer       user-profile-reader
        :item-cursor-prepare user-item-cursor-prepare
        :query-defaults      {:limit         10
                              :offset        0
                              :search-string ""
                              :browse-mode   browse-mode
                              :target        :server}})
     ]))

(defn handle-my
  [module event]
  (let [{:keys [core session-reader]} module
        user-id (api/read-val session-reader [:principal :db/id])]
    (if (= :sys.users/guest user-id)
      (api/pub-event
        core
        {:qn         :authorization/permission-denied
         :denied-msg {:qn :action.app/read-user-profile}})
      (api/pub-event
        core
        {:qn     :action.app/read-user-profile
         :filter {:db/id user-id}
         :browse-mode
                 (case (:qn event)
                   :action.app/my-posts :posts
                   :action.app/my-profile :profile
                   :action.app/my-comments :comments
                   :action.app/my-favorities :favorities
                   :action.app/my-subs :subscribers
                   nil)}))))

(defrecord UsersModule []
  api/IModule
  api/IComponent
  (start [module] module)
  (stop [module] module))

(defn new-users-module [& [m]]
  (map->UsersModule
    (merge
      {api/id-key          :users-module
       api/static-deps-key [:localizer :core :uri-builder :session-reader]
       api/event-subs-key  [{:msg-filter #{:action.app/my-posts
                                           :action.app/my-profile
                                           :action.app/my-comments
                                           :action.app/my-favorities
                                           :action.app/my-subs}
                             :handler    handle-my}]
       api/routes-key      [{:msg-filter      :action.app/browse-user-profiles
                             :route-component user-profiles-browser}
                            {:msg-filter       :action.app/read-user-profile
                             :route-component  user-profile-reader
                             :route-merge-data {:value :loading}}]}
      m)))