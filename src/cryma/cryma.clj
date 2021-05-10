(ns cryma.cryma
  (:require
    [taoensso.sente.server-adapters.immutant :as sente-adapters]
    [cryma.core.api :as api]
    [cryma.core.system :as sys]
    [cryma.core.core :as core]
    [cryma.core.ring-handler-builder :as handler-builder]
    [cryma.core.spa-state-builder :as spa-state-builder]
    [cryma.core.immutant-webserver :as immutant-webserver]
    [cryma.core.in-memory-session-manager :as session-manager]
    [cryma.core.transport :as transport]
    [cryma.core.notifier :as notifier]
    [cryma.core.readers.io-edn-resource :as io-edn-resource]
    [cryma.core.readers.primitive-data-resource :as primitive-resource]
    [cryma.core.localizer :as localizer]
    [cryma.core.spa-handler :as spa]
    [cryma.core.spa-uri-sync :as uri-sync]
    [cryma.core.spa-authenticator :as spa-authenticator]
    [cryma.core.bcrypt-hasher :as bcrypt-hasher]
    [cryma.app.user-directory :as user-directory]
    [cryma.core.datomic-data-provider :as datomic-db]
    [cryma.core.logger :as logger]
    [cryma.core.dev-logger-pipe :as dlp]
    [cryma.core.access-control-pipe :as acp]
    [cryma.core.request-validator-pipe :as rvp]
    [cryma.core.msg-filters :as mf]
    [cryma.app.authorizer :as authorizer]
    [cryma.app.db :as db]
    [cryma.app.app :as app]
    [cryma.btcgate.btc-gate :as btc]
    [cryma.app.billing :as bill]
    [cryma.app.qr-service :as qr]
    [cryma.app.currency-quotes-provider :as cqp]
    [cryma.app.helpdesk :as helpdesk]
    [datomic.api :as d]
    [clojure.java.jdbc :as j]
    [cryma.app.comments :as comments]
    [cryma.core.dtl.transformers.str :as str]
    [cryma.core.dtl.core :as dtl]
    [cryma.core.dtl.transformers.mapping :as mapping]
    [cryma.core.env-config :as env]
    [clojure.core.async :as async])

  (:gen-class)
  (:import (java.util Date)))

(let [stand-mode (env/conf :stand-mode :prod)
      prod? (= stand-mode :prod)
      setup? (or (not prod?) (some? (env/conf :is-setup)))]
  (def sys
    (sys/build-system

      {:localizer           {localizer/default-locale-key "ru"}
       :immutant-web-server {:host (env/conf :server-host "localhost")
                             :port (env/conf :server-port 8080)}
       :billing             {:base-currency-price-precision 3
                             :await-payment-timeout-minutes (env/conf :await-payment-timeout-minutes (if prod? 30 2))}
       :spa                 {api/stand-mode-key stand-mode}}
      (logger/new-logger {logger/ignore-units-key #{:notifier :session-manager}
                          api/configuration-key   {api/log-level-key :debug}})
      (core/new-core)
      (session-manager/new-in-memory-session-manager
        {api/configuration-key
         {:expiration-params         {:session-watch-interval-ms 30000
                                      :session-idle-expire-min   (* 1 60)}
          :prolongation-event-filter :transport/ping
          :drop-event-filter         :transport/session-empty
          :empty-drop-timeout-ms     30000}})
      (transport/new-client-server-transport
        sente-adapters/sente-web-server-adapter "/chsk"
        {api/deps-map-key {:session-reader :session-manager}})
      (notifier/new-notifier)
      (handler-builder/new-ring-handler-builder
        {api/static-deps-key [handler-builder/session-store-key]
         api/deps-map-key    {handler-builder/session-store-key :session-manager}})
      (immutant-webserver/new-immutant-web-server
        {api/configuration-key {:port 8080 :host "localhost"}})
      (localizer/new-localizer
        [(io-edn-resource/new-reader
           "locales/common-ru.edn" :read-on-construction!)
         (io-edn-resource/new-reader
           "locales/helpdesk-ru.edn" :read-on-construction!)])
      (spa-state-builder/new-client-state-builder)
      (spa/new-spa-handler
        (primitive-resource/new-reader
          {:app-id       :app/app
           :app-name     "Freespeach"
           :build-number "0.1.0"
           :logo-hiccup  [:span [:span.title "Freespeach"]]
           :main-ns      "cryma.cryma"})
        [:app-cache])
      (spa-authenticator/new-spa-authenticator
        {spa-authenticator/principal->user-id-fn :db/id
         spa-authenticator/guest-user-key        :sys.users/guest})
      (uri-sync/new-spa-uri-sync
        {:query-params-type-rules
         {:limit  :long
          :offset :long}})
      (bcrypt-hasher/new-hasher "HmAcSecreTKey-[999]")
      (user-directory/new-user-directory
        {api/deps-map-key {:password-hasher :hasher}})
      (datomic-db/new-data-provider
        {api/configuration-key {datomic-db/uri-key
                                (if prod?
                                  (env/conf :app-conn-str
                                            "datomic:sql://prod-app?jdbc:h2:tcp://localhost/~/test;USER=datomic;PASSWORD=MyPassword")
                                  "datomic:mem://dev-app")}
         datomic-db/default-filter-ctors-key
                               [(partial db/exclude-attrs-filter-ctor #{:user/pwd-hash :user/login})]
         datomic-db/create-onstart-key
                               setup?
         datomic-db/schema-sources-key
                               (if prod?
                                 [(io-edn-resource/new-reader
                                    "datomic/app-schema.edn")
                                  (io-edn-resource/new-reader
                                    "datomic/app-prod-users.edn")
                                  (io-edn-resource/new-reader
                                    "datomic/billing.edn")
                                  (io-edn-resource/new-reader
                                    "datomic/helpdesk.edn")]
                                 [(io-edn-resource/new-reader
                                    "datomic/app-schema.edn")
                                  (io-edn-resource/new-reader
                                    "datomic/app-test-users.edn")
                                  (io-edn-resource/new-reader
                                    "datomic/app-test-hubs.edn")
                                  (io-edn-resource/new-reader
                                    "datomic/billing.edn")
                                  (io-edn-resource/new-reader
                                    "datomic/helpdesk.edn")])})
      (datomic-db/new-data-provider
        {api/id-key            :btc-gate-data-provider
         api/configuration-key {datomic-db/uri-key
                                (if prod?
                                  (env/conf :btc-gate-conn-str
                                            "datomic:sql://prod-btc-gate?jdbc:h2:tcp://localhost/~/test;USER=datomic;PASSWORD=MyPassword")
                                  "datomic:mem://dev-btc-gate")}
         datomic-db/create-onstart-key
                               setup?
         datomic-db/schema-sources-key
                               [(io-edn-resource/new-reader
                                  "datomic/btc-gate.edn")
                                (io-edn-resource/new-reader
                                  "datomic/btc-gate-forwarding-addrs.edn")]})
      #_(btc/new-btc-gate
        {api/deps-map-key {:data-provider :btc-gate-data-provider}
         api/configuration-key
                          {:network                    (if prod? :btc-network/prod :btc-network/test)
                           :wallet-name                "wallet"
                           :wallet-path                "."
                           :wallet-log-level           :error
                           :btc-confidence-level       1
                           :btc-confidence-level-limit 7}})
      (qr/new-qr-service)
      (bill/new-billing
        {api/configuration-key
                     {:base-currency-price-precision 3
                      :await-payment-timeout-minutes 2}
         :faq-source (io-edn-resource/new-reader
                       "public/faq.edn")})
      (cqp/new-currency-quotes-provider
        {api/configuration-key
         {:refresh-quotes-interval-minutes 70}
         :currency-quotes-providers
         [(cqp/new-openexchangerates-org-quotes-provider
            "https://openexchangerates.org/api/latest.json?app_id=d98b9379404b450a96e261b6020ce566")
          (cqp/new-currencylayer-com-quotes-provider
            "http://www.apilayer.net/api/live?access_key=9a0d6b9919592b58704e77252dbbe8c0")
          ]})

      (helpdesk/new-helpdesk
        {api/deps-map-key {:spa-handler :spa}})

      ;;PIPES start
      ;;-----------------------------------------
      (dlp/new-dev-logger-pipe
        {:pipe-priority         100
         :ignore-event-filter   {:qn #{:transport/ping :authorizer/authorize-event}}
         :ignore-request-filter {:qn #{:authorizer/authorize-event
                                       :authorizer/authorize-request
                                       :authorizer/authorize-subscribtion
                                       :notification/subscribe}}})
      (rvp/new-validation-pipe
        {:pipe-priority 99})
      (acp/new-acp
        {:pipe-priority 0
         acp/request-group-strategies-key
                        [{:msg-filter {:nsqn :localizer}
                          :strategy   :allow}
                         {:msg-filter {:nsqn :notifier}
                          :strategy   [:call-async :authorizer/authorize-subscription]}
                         {:msg-filter {:nsqn :notifier}
                          :strategy   :allow}
                         {:msg-filter {:nsqn :spa-authenticator}
                          :strategy   :allow}
                         {:msg-filter {:nsqn :app}
                          :strategy   :allow}
                         {:msg-filter {:nsqn :helpdesk}
                          :strategy   :allow}
                         {:msg-filter {:nsqn :billing}
                          :strategy   :allow}
                         {:msg-filter {:nsqn :promo}
                          :strategy   :allow}
                         {:msg-filter {:nsqn :action.app}
                          :strategy   [:call-async :authorizer/authorize-request]}
                         {:msg-filter {:nsqn :action.helpdesk}
                          :strategy   [:call-async :authorizer/authorize-request]}
                         {:msg-filter {:nsqn :action.adm}
                          :strategy   [:call-async :authorizer/authorize-request]}
                         {:msg-filter mf/empty-filter
                          :strategy   :allow-local}]
         acp/event-group-strategies-key
                        [{:msg-filter {:qn :event.app/tracker-event-read}
                          :strategy   :allow}
                         {:msg-filter {:qn :notifier/unsubscribe}
                          :strategy   :allow}
                         {:msg-filter mf/empty-filter
                          :strategy   :allow-local}]})
      #_(dlp/new-dev-logger-pipe
          {api/id-key     :after-logger
           :pipe-priority -1})
      ;;PIPES end
      ;;-----------------------------------------
      (authorizer/new-authorizer
        {authorizer/nsqn-key #{:action.app :action.adm :action.helpdesk}})
      (app/new-app {api/deps-map-key {:spa-handler :spa}}))))


#_(:sys.component/static-dependencies (:client-state-builder sys))

#_(:order-deps (:client-state-builder sys))

#_(:sys.component/static-dependencies (:spa sys))

#_(def started (sys/start-system sys))

#_(sys/stop-system started)

#_(keys started)
#_(keys (:notifier started))
#_(keys (:core started))
#_(:connected-uids (:transport started))
#_(keys (:ring-handler-builder started))
#_(keys (:session-manager started))
#_(get-in started [:ring-handler-builder :session-store])
#_(get-in started [:session-manager :session-manager/state])
#_(api/get-conf (:localizer started) localizer/locale-request-path-key)

#_(cqp/provide-quotes
    (cqp/new-openexchangerates-org-quotes-provider
      "https://openexchangerates.org/api/latest.json?app_id=d98b9379404b450a96e261b6020ce566")
    [{:db/ident             :currency/bitcoin
      :currency/letter-code "BTC"}]
    )

#_(def db {:classname   "org.h2.Driver"                     ; must be in classpath
           :subprotocol "h2"
           :subname     "jdbc:h2:tcp://localhost/~/test"
           ; Any additional keys are passed to the driver
           ; as driver-specific properties.
           :user        "sa"
           :password    ""})

#_(j/query db ["select 1;"])

#_(d/create-database
    "datomic:sql://darusoc?jdbc:h2:tcp://localhost/~/test;USER=datomic;PASSWORD=MyPassword")

#_(:currency-quotes-providers (:currency-quotes-provider started))

#_(cqp/refresh-quotes
    (:currency-quotes-provider started))

#_(count @(:subs (notifier/subs-key (:notifier started))))
#_(api/pub-event
    (:core started)
    {:qn     :notif
     :target :notifier/subscribers
     :kakaka :pupupu})

#_(api/pub-event
    (:core started)
    {:qn          :notifications.events/comment-changed
     :comment     {}
     :anchor-path [307863255778401]
     :target      :notifier/subscribers})

#_(time ((:localizer started)
          {:params {:locale "ru"}}
          {:code :date/since-minutes :since-minutes 5}))


#_(require '[clojure.core.async :as async])


#_(time
    (do
      (async/<!! (api/call-async
                   (:core started)
                   (with-meta
                     {:qn    :action.app/browse-posts
                      :limit 10 :offset 0 :target :server}
                     {:remote?  true
                      :ring-req {:session {:user-id 316659348800586}}})))
      nil))

#_(let [ids [325455441822798 325455441822797 325455441822796 325455441822791]
        cnt (count ids)]
    (dotimes [tm 200]
      (async/thread
        (time
          (doseq [numb (range 100)]
            (async/<!! (api/call-async
                         (:core started)
                         (with-meta
                           {:qn    :action.app/browse-posts
                            :limit 10 :offset 0 :target :server}
                           {:remote?  true
                            :ring-req {:session {:user-id (get ids (rand-int cnt))}}}))))
          ))))

#_(async/<!! (api/call-async
               (:core started)
               {:qn :promo/get-promo-data}))

#_(async/<!! (api/call-async
               (:core started)
               {:qn     :action.app/read-post-discussion,
                :filter {:sys/anchor 334251534845102},
                :target :server}
               ))

#_(async/<!! (api/call-async
               (:core started)
               (with-meta
                 {:limit 15, :offset 0, :user-id 316659348800702, :target :server, :qn :action.app/read-user-tracker}
                 {:auth-result {:user-id 316659348800702}})
               ))

#_(async/<!! (api/call-async
               (:core started)
               ))

#_(let [ch (async/chan)
        r (with-meta
            {:qn                 :app/change-credentials
             :user/login-current "test-e"
             :user/pwd-current   "12345678"
             :user/pwd           "11112222"}
            {:response-chan ch
             :auth-result   {:user-id 316659348800587}})]
    (app/handle-change-credentials (:app started) r)
    (async/<!! ch))

#_(->>
    (d/q '[:find (pull ?order [:* {:sys/status [:db/ident]}])
           :in $ ?now-time
           :where
           [?order :order/expiry-date ?expiry]
           [?order :sys/status ?status]
           [?status :db/ident ?status-ident]
           [(.getTime ^java.util.Date ?expiry) ?expiry-time]
           [(<= ?expiry-time ?now-time)]
           [(not= ?status-ident :order-statuses/cancelled)]
           ]
         (api/get-db (:data-provider started))
         (.getTime (java.util.Date.)))
    (mapv first))

#_(async/<!! (api/call-async
               (:core started)
               {:qn       :btc-gate/request-payment
                :order-id "2243"
                :amount   0.001M}))

#_(async/<!! (api/call-async
               (:core started)
               {:qn                :user-directory/create-user
                :sys/author        :sys.users/billing
                :user/login        ""
                :user/pwd          ""
                :user/nickname     ""
                :user/access-level :access-level/zeta
                :user/is-sys       true
                }))

#_(async/<!! (api/call-async
               (:core started)
               {:qn                :user-directory/update-user
                :db/id             316659348800668
                :sys/author        :sys.users/billing
                :user/access-level :access-level/gamma
                :user/login        "ddp"
                }))

#_(async/<!! (api/call-async
               (:core started)
               {:qn               :user-directory/update-user-credentials
                :db/id            316659348800668
                :sys/author       :sys.users/billing
                :user/login       "kaha"
                :user/pwd-current "kaha-14"
                }))

#_(async/<!! (api/call-async
               (:core started)
               {:qn                  :billing/process-order
                :order/account-data  {:user/login    ""
                                      :user/pwd      ""
                                      :user/nickname ""}
                :order/product       :access-level/zeta
                :order/product-price 549755813889170
                :order/paygate       :paygates/btc-gate
                }))

#_(async/<!! (api/call-async
               (:core started)
               {:qn                  :billing/process-order
                :order/account       316659348800676
                :order/product       :access-level/zeta
                :order/product-price 549755813889169
                :order/paygate       :paygates/btc-gate
                }))


#_(async/<!! (api/call-async
               (:core started)
               {:qn                  :billing/calculate-order
                :order/account       316659348800676
                :order/product       :access-level/zeta
                :order/product-price 549755813889169
                :order/paygate       :paygates/btc-gate
                }))

#_(async/<!! (api/call-async
               (:core started)
               {:qn                  :billing/calculate-order
                :order/product       :access-level/gamma
                :order/product-price 549755813889174
                :order/paygate       :paygates/btc-gate
                }))

#_(async/<!! (api/call-async
               (:core started)
               {:qn                  :billing/calculate-order
                :order/product       :access-level/zeta
                :order/product-price 549755813889169
                :order/paygate       :paygates/btc-gate
                }))
#_(async/<!! (api/call-async
               (:core started)
               {:qn         :user-directory/remove-user
                :sys/author :sys.users/billing
                :db/id      316659348800687}))

#_(async/<!! (api/call-async
               (:core started)
               {:qn      :user-directory/get-user-info
                :user-id 316659348800668
                }))

#_(->
    (:data-provider started)
    (api/get-db)
    (d/entity :sys.users/guest)
    :db/ident)

#_(->
    (:data-provider started)
    (api/get-db)
    (d/pull [:*] :access-category/helpdesk)
    )

#_(bill/get-last-order
    (api/get-db (:data-provider started))
    316659348800680
    )

#_(bill/get-product-prices
    (:billing started)
    )

#_(d/pull (api/get-db (:data-provider started))
          [:* {:access-level/permission [:* {:permission/access-category  [:db/ident]
                                             :permission/permitted-action [:db/ident]}]}]
          :access-level/omega-authenticated)

#_(d/entity (api/get-db (:data-provider started))
            316659348800668)

#_(user-directory/get-users
    (api/get-db (:data-provider started))
    {})

#_(bill/do-calculate-order
    (:billing started)
    {:order/product       :access-level/gamma
     :order/product-price 549755813889168
     :order/paygate       :paygates/btc-gate})

#_(bill/do-calculate-order
    (:billing started)
    {:order/product       :access-level/gamma
     :order/product-price 549755813889174
     :order/paygate       :paygates/btc-gate})

#_(d/pull
    (api/get-db (:data-provider started))
    [:*]
    [:product/service-key :access-level/zeta]
    )

#_(d/pull
    (api/get-db (:data-provider started))
    [:*]
    [:product/service-key :access-level/gamma]
    )


#_(btc/send-coins (:btc-gate started) "n1QFVoJG4f55UJMAkrZWeJHb53pq2f3gUQ" 0.0001M)
#_(btc/send-coins (:btc-gate started) "2NCsZXDCPb71uo58FJPDXvJXyWzmjtx73zN" 0.0001M)

#_(btc/get-wallet (:btc-gate started))

#_(let [db (api/get-db (:btc-gate-data-provider started))]
    (d/pull db [:*] [:payment-request/address "n1KfVCsP7iBdeNAJrNPs2qVhcZ8gkCYHsz"]))





#_(cryma.app.anchored/get-anchored-vals
    (api/get-db (:data-provider started))
    {:entity #{:entity.app/comment :entity.app/sub :entity.app/rating :entity.app/favorite}})


#_(d/q
    '[:find (pull ?e [:* {:currency-quote/foreign [:db/ident]}])
      :in $
      :where [?e :currency-quote/foreign]]
    (api/get-db (:data-provider started)))

#_(d/q
    '[:find (pull ?e [:*])
      :in $
      :where [?e :sys/entity :entity.sys/access-category]]
    (api/get-db (:data-provider started)))


#_(d/pull
    (api/get-db (:data-provider started))
    [:* {:sys/_anchor
         [:* {:sys/author [:db/id :user/nickname]} {:sys/entity [:db/ident]}
          {:sys/_anchor '...}]}]
    329853488333929)

#_(async/<!! (api/call-async
               (:core started)
               {:qn      :authorizer/authorize-request
                :user-id nil
                :msg     {:qn :action.app/browse-posts}}))

(def started-state (atom nil))

(defn -main []
  (let [started (sys/start-system sys)]
    (reset! started-state started)))


