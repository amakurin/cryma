(ns cryma.cryma
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require
    [cljs.core.async :refer [<! put! >! alts!]]
    [cljs.reader :as reader]
    [weasel.repl :as repl]
    [cryma.core.api :as api]
    [cryma.core.system :as sys]
    [cryma.core.core :as core]
    [cryma.core.localizer :as localizer]
    [cryma.core.transport :as transport]
    [cryma.core.client-notifier :as notifier]
    [cryma.core.spa :as spa]
    [cryma.core.spa-router :as spa-router]
    [cryma.core.spa-startup :as startup]
    [cryma.core.spa-uri-sync :as uri-sync]
    [cryma.core.spa-authenticator :as spa-authenticator]
    [cryma.core.default-auth-screen :as default-auth-screen]
    [cryma.core.logger :as logger]
    [cryma.core.dev-logger-pipe :as dlp]
    [cryma.core.access-control-pipe :as acp]
    [cryma.core.request-validator-pipe :as rvp]
    [cryma.app.authorizer :as authorizer]
    [cryma.app.posts :as posts]
    [cryma.app.hubs :as hubs]
    [cryma.app.users :as users]
    [cryma.app.promo :as promo]
    [cryma.app.tracker :as tracker]
    [cryma.core.msg-filters :as mf]
    [cryma.app.adm :as adm]
    [cryma.app.helpdesk :as hd]))

(enable-console-print!)

(when-not (repl/alive?)
  (try
    (repl/connect "ws://localhost:9002")
    (catch :default _)))

(def dev-sys (atom {}))

(defn ^:export main [serialized-config]
  (let [configuration (reader/read-string serialized-config)
        sys (sys/build-system
              configuration
              (logger/new-logger {logger/ignore-units-key #{:notifier}})
              (core/new-core)
              (localizer/new-localizer)
              (transport/new-client-server-transport)
              (notifier/new-notifier)
              (spa/new-spa
                {api/deps-map-key
                 {:l :localizer}
                 spa/menu-source-key
                 [{:menu-item/id   :app/posts
                   :menu-item/type :menu-item
                   :event          {:qn :action.app/browse-posts}}
                  {:menu-item/id   :app/hubs
                   :menu-item/type :menu-item
                   :event          {:qn :action.app/browse-hubs}}
                  {:menu-item/id   :app/users
                   :menu-item/type :menu-item
                   :event          {:qn :action.app/browse-user-profiles}}
                  {:menu-item/type :menu-divider}
                  {:menu-item/id   :app/create-post
                   :menu-item/type :menu-item
                   :event          {:qn :action.app/create-post}}
                  {:menu-item/id       :app/profile
                   :menu-item/type     :menu-folder
                   spa/menu-source-key [{:menu-item/id   :app/my-feed
                                         :menu-item/type :menu-item
                                         :event          {:qn          :action.app/browse-posts
                                                          :browse-mode :feed}}
                                        {:menu-item/id   :app/my-profile
                                         :menu-item/type :menu-item
                                         :event          {:qn :action.app/my-profile}}
                                        {:menu-item/id   :app/my-posts
                                         :menu-item/type :menu-item
                                         :event          {:qn :action.app/my-posts}}
                                        {:menu-item/id   :app/my-comments
                                         :menu-item/type :menu-item
                                         :event          {:qn :action.app/my-comments}}
                                        {:menu-item/id   :app/my-favorities
                                         :menu-item/type :menu-item
                                         :event          {:qn :action.app/my-favorities}}
                                        {:menu-item/id   :app/my-subs
                                         :menu-item/type :menu-item
                                         :event          {:qn :action.app/my-subs}}]}
                  {:menu-item/id       :app/tracker
                   :menu-item/type     :menu-folder
                   spa/menu-source-key :tracker-menu-source
                   :notification-tags  #{:event.app/tracker-event}}
                  {:menu-item/type :menu-divider}
                  {:menu-item/id       :app.helpdesk/helpdesk
                   :menu-item/type     :menu-item
                   spa/menu-source-key :helpdesk-customer-menu-source
                   :event              {:qn :action.helpdesk/customer-dashboard}
                   :notification-tags  #{:event.helpdesk/customer-event}}]})
              (spa-router/new-spa-router
                {:default-navigation-prev-event {:qn :action.app/browse-posts}})
              (startup/new-spa-startup
                #_{:start-event {:qn :action.app/create-post}})
              (uri-sync/new-spa-uri-sync
                {:query-params-type-rules
                 {:limit  :long
                  :offset :long}}
                #_{:sync-uri? false})
              (spa-authenticator/new-spa-authenticator
                {api/deps-map-key
                 {:uri-builder :spa-uri-sync}
                 spa-authenticator/authenticated-predicate-key
                 (fn [p] (not= :sys.users/guest (:db/id p :sys.users/guest)))
                 spa-authenticator/username-render-fn-key
                 (fn [p] (:user/nickname p (:db/ident p)))})
              (default-auth-screen/new-authentication-screen
                {api/deps-map-key {:session-reader :spa-authenticator}})

              (dlp/new-dev-logger-pipe
                {:pipe-priority 100})
              (rvp/new-validation-pipe
                {:pipe-priority 99})
              (acp/new-acp
                {:pipe-priority      0
                 acp/event-group-strategies-key
                                     [{:msg-filter #{:action.app/my-posts
                                                     :action.app/my-profile
                                                     :action.app/my-comments
                                                     :action.app/my-favorities
                                                     :action.app/my-subs}
                                       :strategy   :allow}
                                      {:msg-filter {:nsqn :action.app}
                                       :strategy   [:call-async :authorizer/authorize-event]}
                                      {:msg-filter {:nsqn :action.helpdesk}
                                       :strategy   [:call-async :authorizer/authorize-event]}
                                      {:msg-filter {:nsqn :action.adm}
                                       :strategy   [:call-async :authorizer/authorize-event]}
                                      {:msg-filter mf/empty-filter
                                       :strategy   :allow}]
                 acp/request-group-strategies-key
                                     [{:msg-filter mf/empty-filter
                                       :strategy   :allow}]
                 acp/msg->user-id-fn-key
                                     (fn [acp _] (api/read-val (:session-reader acp) [:principal :db/id]))
                 api/static-deps-key [:session-reader]
                 api/deps-map-key    {:session-reader :spa-authenticator}})
              #_(dlp/new-dev-logger-pipe
                  {api/id-key     :dlp-after
                   :pipe-priority -1})

              (authorizer/new-authorizer
                {api/deps-map-key {:session-reader :spa-authenticator}
                 authorizer/action-opts-key
                                  {:action.helpdesk/customer-dashboard
                                   {:category-search-root :access-category/helpdesk}
                                   :action.helpdesk/admin-dashboard
                                   {:category-search-root :access-category/helpdesk}
                                   :action.adm/users
                                   {:category-search-root :access-category/administrative}
                                   :action.adm/hubs
                                   {:category-search-root :access-category/administrative}
                                   :action.adm/billing
                                   {:category-search-root :access-category/administrative}}})
              (hubs/new-hubs-module
                {api/deps-map-key {:uri-builder :spa-uri-sync}})
              (posts/new-posts-module
                {api/deps-map-key {:uri-builder    :spa-uri-sync
                                   :session-reader :spa-authenticator}})
              (users/new-users-module
                {api/deps-map-key {:uri-builder    :spa-uri-sync
                                   :session-reader :spa-authenticator}})
              (promo/new-promo-module
                {api/deps-map-key {:uri-builder    :spa-uri-sync
                                   :session-reader :spa-authenticator}})
              (tracker/new-tracker-module
                {api/deps-map-key {:uri-builder    :spa-uri-sync
                                   :session-reader :spa-authenticator}})
              (hd/new-helpdesk
                {api/deps-map-key {:uri-builder    :spa-uri-sync
                                   :session-reader :spa-authenticator}})
              (adm/new-adm
                {api/deps-map-key {:uri-builder    :spa-uri-sync
                                   :session-reader :spa-authenticator}}))
        started (sys/start-system sys)]

    (reset! dev-sys started)))


#_(keys @dev-sys)
#_(:spa-uri-sync @dev-sys)
#_(keys (:authorizer @dev-sys))
#_(:authorizer/request-settings (:authorizer @dev-sys))

#_(:spa-authenticator/state (:spa-authenticator @dev-sys))

#_(:validator/request-settings (:validation-pipe @dev-sys))
#_(:notifier/subs (:notifier @dev-sys))


#_(api/read-val (:session-reader (:authorizer @dev-sys)) [])

#_(let [l (:localizer @dev-sys)]
    (l {:code :date/since-minutes :since-minutes 5}))

#_(go
    (let [res (<! (api/call-async
                    (:core @dev-sys)
                    {:qn  :authorizer/authorize-request
                     :msg {:qn :action.app/write-comment,
                           :value {:sys/author 316659348800679, :comment/body "<html></html>",
                                   :sys/anchor 334251534845102}, :target :server}}))]
      (enable-console-print!)
      (println "=-=-=-=-=-" res)))

#_(go
    (let [res (<! (api/call-async
                    (:core @dev-sys)
                    {:qn                 :authorizer/pre-authorize-action
                     :action-id          :action.app/edit-comment
                     :access-category-id :access-category/open
                     :sys/author         303465209267258}))]
      (enable-console-print!)
      (println "=-=-=-=-=-" res)))

#_(go
    (let [res (<! (api/call-async
                    (:core @dev-sys)
                    {:qn      :notifier/subscribe
                     :filters [{:qn :notif}]
                     :target  :server}))]
      (enable-console-print!)
      (println "=-=-=-=-=-" res)))

#_(api/pub-event
    (:core @dev-sys)
    {:qn                :notifier/unsubscribe
     :subscription/uuid #uuid "c7154a90-5adf-437a-8dab-cd65c652f6b7"
     :target            :server})
