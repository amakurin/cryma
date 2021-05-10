(ns cryma.app.billing
  (:require [cryma.core.api :as api]
            [datomic.api :as d]
            [clojure.core.async :as async]
            [clojure.tools.reader.edn :as edn]
            [cryma.app.db :as db]
            [cryma.core.tools :as tools]
            [crypto.random :as cryptorand])
  (:import (java.util Date Calendar TimeZone)
           (java.util.concurrent TimeUnit)
           (java.math RoundingMode MathContext)))

(defn auth-result [request & ks]
  (let [auth-result (:auth-result (meta request))]
    (if (seq ks)
      (get-in auth-result ks)
      auth-result)))

(defn make-user-request [user-id]
  {:qn      :user-directory/read-user
   :pattern [:db/id
             :user/login
             :user/access-expiry
             {:user/access-level [:db/id :db/ident]}]
   :filter  {:db/id user-id}})

(defn account-id? [request]
  (let [account-id (auth-result request :user-id)]
    (when (and account-id
               (not= account-id :sys.users/guest))
      account-id)))

(defn get-calc-account [billing request]
  (let [core (:core billing)
        account-id (account-id? request)
        response (when account-id
                   (async/<!!
                     (api/call-async core (make-user-request account-id))))]
    (when (api/success? core response)
      (:result response))))

(defn get-product [db product-id]
  (when product-id
    (d/pull db [:* {:product/pricing [:* {:product-price/time-frame [:*]}]}]
            (if (keyword? product-id)
              [:product/service-key product-id]
              product-id))))

(defn get-product-price [product price-id]
  (some (fn [price]
          (when (= (:db/id price) price-id) price))
        (:product/pricing product)))

(defn get-paygate [db paygate-id]
  (d/pull db [:* {:paygate/currency
                  [:* {:currency-quote/_foreign [:*]}]}]
          paygate-id))

(defn get-last-order [db account-id]
  (->>
    (d/q
      '[:find (pull ?order
                    [:*
                     {:sys/status [:db/id :db/ident]}
                     {:order/product
                      [:* {:product/pricing
                           [:* {:product-price/time-frame [:*]}]}]}]) (pull ?tx [:*])
        :in $ ?acc
        :where [?order :order/account ?acc ?tx]]
      db account-id)
    (map (fn [[order tx]] (assoc order :db/txInstant (:db/txInstant tx))))
    (sort-by (fn [order] (.getTime (:db/txInstant order))) >)
    first))

(defn days-since-now [instant]
  (if (nil? instant)
    0
    (let [now (.getTime (Date.))
          future (.getTime instant)]
      (if (>= now future)
        0
        (.convert TimeUnit/DAYS (- future now) TimeUnit/MILLISECONDS)))))

(defn calc-days-cost [days product-price]
  (if (nil? product-price)
    0
    (let [tfdays (bigdec (:time-frame/days (:product-price/time-frame product-price)))
          tfprice (:product-price/value product-price)
          daily-cost (.divide tfprice tfdays 12 RoundingMode/HALF_DOWN)]
      (* daily-cost days))))

(defn base-foreign-quote [foreign-currency]
  (->
    foreign-currency
    :currency-quote/_foreign
    first
    :currency-quote/base-to-foreign-quote))

(defn to-precision [bigdec-value precision]
  (let [workaround-zero-bug? (= 0 (.toBigInteger bigdec-value))
        bigdec-value (if workaround-zero-bug? (+ bigdec-value 1) bigdec-value)]
    (->
      bigdec-value
      (.round (MathContext. precision RoundingMode/HALF_UP))
      (.stripTrailingZeros)
      (tools/->if workaround-zero-bug? #(- % 1)))))

(defn base->foreign-amount [base-amount foreign-currency]
  (let [quote (base-foreign-quote foreign-currency)]
    (* base-amount quote)))

(defn base->paygate-amount [base-amount paygate]
  (let [{:keys [:paygate/currency :paygate/discount :paygate/precision]} paygate
        base-amount
        (if discount (* base-amount (- 1 discount)) base-amount)
        paygate-amount
        (if currency (base->foreign-amount base-amount currency) base-amount)]
    {:amount         base-amount
     :discount       (or discount 0)
     :paygate-amount (to-precision paygate-amount precision)}))

(defn check-found [field value]
  (if (or (nil? value) (nil? (:db/id value))
          (empty? (set (remove #{:db/id} (keys value)))))
    (throw (ex-info "Wrong field value"
                    {:code :field-value-not-found :field field}))
    value))

(defn do-calculate-order [billing request]
  (let [{:keys [data-provider]} billing
        {:keys [:order/product
                :order/product-price
                :order/paygate]} request
        db (api/get-db data-provider)
        account (get-calc-account billing request)
        {:keys [:user/access-level
                :user/access-expiry]} account
        current-product
        (when-let [access-level (:db/ident access-level)]
          (get-product db [:product/service-key access-level]))
        current-product-price
        (when access-level
          (when-let [last-order (get-last-order db (:db/id account))]
            (get-product-price
              current-product
              (get-in last-order [:order/product-price :db/id]))))
        product (check-found :order/product (get-product db product))
        product-price (check-found :order/product-price (get-product-price product product-price))
        paygate (check-found :order/paygate (get-paygate db paygate))
        total-days (get-in product-price
                           [:product-price/time-frame :time-frame/days])
        payed-days (if access-level (days-since-now access-expiry) 0)
        unpayed-days (- total-days payed-days)
        base-amount (+ (calc-days-cost payed-days product-price)
                       (- (calc-days-cost payed-days current-product-price))
                       (calc-days-cost unpayed-days product-price))
        result (base->paygate-amount base-amount paygate)
        base-precision (api/get-conf billing :base-currency-price-precision 3)]
    #_(println "-------calculation------ "
               "\ntotal-days" total-days
               "\npayed-days" payed-days
               "\nunpayed-days" unpayed-days
               "\ncurrent-product" access-level
               "\ncurrent-product-price" current-product-price
               "\npayed-cost" (calc-days-cost payed-days current-product-price))
    (->
      result
      (update :amount to-precision base-precision)
      (assoc
        :base-amount (to-precision (calc-days-cost total-days product-price) base-precision)
        :access-level (:product/service-key product)
        :days total-days))))


(defn serve-calculate-order [billing request]
  (let [{:keys [core logger]} billing]
    (try
      (let [calculation-result (do-calculate-order billing request)]
        (api/respond-success core request calculation-result))
      (catch Exception e
        (let [data (ex-data e)]
          (logger :error "Exception:" (.getMessage e) " Ex-data" (pr-str data))
          (if (= :field-value-not-found (:code data))
            (api/respond-error core request (select-keys data [:code :field]))
            (throw e)))))))

(defn resolve-account [billing request]
  (if (account-id? request)
    (get-calc-account billing request)
    (let [{:keys [core data-provider]} billing
          db (api/get-db data-provider)
          account-data (:order/account-data request)
          product (:order/product request)
          response (async/<!!
                     (api/call-async
                       core
                       {:qn                  :user-directory/create-user
                        :sys/author          :sys.users/billing
                        :user/access-level   (if (keyword? product)
                                               product
                                               (:product/service-key (d/entity db product)))
                        :user/login          (:user/login account-data)
                        :user/pwd            (:user/pwd account-data)
                        :user/nickname       (:user/nickname account-data)
                        :sys/access-category :access-category/administrative
                        :user/is-sys         true}))]
      (if (api/success? core response)
        (assoc (:result response) :temporary? true)
        (throw (ex-info "Account creation errors"
                        {:code   :account-creation-error
                         :errors (:errors response)}))))))

(defn add-days-to-now [days]
  (let [now (Calendar/getInstance (TimeZone/getTimeZone "UTC"))]
    (.add now Calendar/DATE days)
    (.getTime now)))

(defn provisioning-account-activate [billing account-update-data]
  (let [{:keys [core]} billing
        {:keys [account access-level days temporary-account?]} account-update-data
        response (async/<!!
                   (api/call-async
                     core
                     (->
                       {:qn                 :user-directory/update-user
                        :sys/author         :sys.users/billing
                        :db/id              account
                        :user/access-level  access-level
                        :user/access-expiry (add-days-to-now days)
                        :user/is-sys        false}
                       (tools/->if temporary-account? assoc :sys/access-category :access-category/open))))]
    (if (api/success? core response)
      (:result response)
      (throw (ex-info "Account activation errors"
                      {:code   :account-activation-error
                       :errors (:errors response)})))))

(defn order-initial-status [order-data]
  (if (<= (:order/amount order-data) 0)
    :order-statuses/confirmed
    :order-statuses/pending))

(defn order-expiry-instant [billing & [from-date]]
  (let [minutes (api/get-conf billing :await-payment-timeout-minutes 60)
        now (Calendar/getInstance (TimeZone/getTimeZone "UTC"))]
    (when from-date
      (.setTime now from-date))
    (.add now Calendar/MINUTE minutes)
    (.getTime now)))

(defn create-order [billing request]
  (let [{:keys [data-provider]} billing
        conn (api/get-conn data-provider)

        tx-data (-> request
                    (tools/->if (nil? (:order/temporary-account? request))
                                dissoc :order/temporary-account?)
                    (select-keys [:order/account
                                  :order/product
                                  :order/product-price
                                  :order/paygate
                                  :order/amount
                                  :order/paygate-amount
                                  :order/remote-address
                                  :order/temporary-account?])
                    (update :order/product
                            (fn [prod]
                              (if (keyword? prod)
                                (:db/id (d/entity (d/db conn) [:product/service-key prod]))
                                prod)))
                    (assoc :order/token (cryptorand/base64 128))
                    (assoc :sys/status (order-initial-status request))
                    (assoc :sys/date (Date.))
                    (assoc :sys/entity :entity.billing/order)
                    (assoc :order/expiry-date (order-expiry-instant billing))
                    (assoc :db/id #db/id[:db.part/billing]))
        tx-result @(d/transact conn [tx-data])
        db (:db-after tx-result)
        dbid (db/get-tx-id tx-data tx-result)]
    (d/pull db [:*
                {:order/paygate [:db/ident]}
                {:sys/status [:db/ident]}] dbid)))

(defn paygate-dispatch [_ order]
  (get-in order [:order/paygate :db/ident]))

(defmulti process-paygate paygate-dispatch)

(defmethod process-paygate :paygates/btc-gate
  [billing order]
  (let [{:keys [core data-provider]} billing
        {order-id :db/id amount :order/paygate-amount} order
        response (async/<!! (api/call-async
                              core
                              {:qn       :btc-gate/request-payment
                               :order-id (str order-id)
                               :amount   amount}))
        paygate-result (if (api/success? core response)
                         (:result response)
                         (throw (ex-info "Btc-gate error" {:code    :paygate-error
                                                           :paygate :paygates/btc-gate
                                                           :errors  (:errors response)})))
        btc-address (get-in paygate-result [:payment-details :address])
        order (assoc order :order/paygate-order-ref btc-address)]
    (d/transact (api/get-conn data-provider)
                [[:db/add order-id :order/paygate-order-ref btc-address]])
    order))

(defmethod process-paygate :default
  [billing order]
  (throw (ex-info "Unsupported paygate"
                  {:code    :unsupported-paygate
                   :paygate (paygate-dispatch billing order)})))

(defn do-process-order [billing request]
  (let [{:keys [core logger]} billing
        {:keys [remote-address]} (meta request)
        _ (when-not remote-address (throw (ex-info "Unknown remote client"
                                                   {:code :unknown-remote-client})))
        {:keys [amount paygate-amount days access-level]}
        (do-calculate-order billing request)
        {account-id :db/id temporary-account? :temporary?}
        (check-found :order/account (resolve-account billing request))
        request (assoc request
                  :order/account account-id
                  :order/amount amount
                  :order/paygate-amount paygate-amount)
        order (create-order billing (assoc request
                                      :order/remote-address (pr-str remote-address)
                                      :order/temporary-account? temporary-account?))]
    (if (#{:order-statuses/confirmed} (get-in order [:sys/status :db/ident]))
      (do
        (provisioning-account-activate
          billing {:account            account-id
                   :access-level       access-level
                   :days               days
                   :temporary-account? temporary-account?})
        order)
      (process-paygate billing order))))

(defn ex-data->error [data keys-to-select & [field]]
  (-> data
      (select-keys keys-to-select)
      (tools/->if field assoc :field field)))

(defn serve-process-order [billing request]
  (let [{:keys [core]} billing]
    (try
      (let [order (do-process-order billing request)]
        (api/respond-success core request order))
      (catch Exception e
        (let [data (ex-data e)]
          (case (:code data)
            :unsupported-paygate
            (api/respond-error core request
                               (ex-data->error data [:code :paygate] :order/paygate))
            :paygate-error
            (api/respond-error core request (:errors data))
            :field-value-not-found
            (api/respond-error core request
                               (ex-data->error data [:code :field]))
            :account-creation-error
            (api/respond-error core request (:errors data))
            :account-activation-error
            (api/respond-error core request (:errors data))
            (throw e)))))))

(defn serve-prolong-order [billing request]
  (let [{:keys [core data-provider]} billing
        conn (api/get-conn data-provider)]
    (d/transact conn
                [[:db/add [:order/token (:order/token request)]
                  :order/expiry-date (order-expiry-instant billing)]])
    (api/respond-success core request)))

(defn order->cancel-info [{:keys [:db/id :order/account :order/temporary-account? :order/remote-address]}]
  {:order-id             id
   :remote-address       remote-address
   :tx-data              [:db/add id :sys/status :order-statuses/cancelled]
   :account-id-to-remove (when temporary-account? account)})

(defn process-expired-order [billing order]
  (let [{:keys [core logger]} billing
        {:keys [order-id tx-data account-id-to-remove remote-address]} order]
    (when account-id-to-remove
      (let [response
            (async/<!! (api/call-async
                         core
                         {:qn         :user-directory/remove-user
                          :sys/author :sys.users/billing
                          :db/id      account-id-to-remove}))]
        (when-not (api/success? core response)
          (logger :error "Billing can't remove temporary user:" account-id-to-remove
                  "Order id:" order-id
                  "Got errors:" (pr-str (:errors response))))))
    (async/<!! (api/call-async
                 core
                 {:qn      :transport/send-to!
                  :address (read-string remote-address)
                  :data    {:qn       :billing/order-cancelled
                            :order-id order-id}}))
    tx-data))

(defn serve-cancel-order [billing request]
  (let [{:keys [core data-provider]} billing
        conn (api/get-conn data-provider)
        db (d/db conn)
        token (:order/token request)
        db-after (when-let [order (d/pull db
                                          [:db/id :order/account
                                           :order/temporary-account?
                                           :order/remote-address
                                           {:sys/status [:db/ident]}]
                                          [:order/token token])]
                   (when (#{:order-statuses/pending :order-statuses/partially-paid}
                           (get-in order [:sys/status :db/ident]))
                     (let [cancel-info (order->cancel-info order)
                           tx-data (process-expired-order billing cancel-info)
                           tx-result @(d/transact conn [tx-data])]
                       (:db-after tx-result))))]

    (api/respond-success
      core request
      (d/pull (or db-after db)
              [{:sys/status [:*]}]
              [:order/token token]))))

(defn find-payment-order [db payment-details]
  (let [order (d/pull db
                      [:db/id
                       :order/paygate-amount
                       :order/paygate-received-amount
                       :order/paygate-confirmed-amount
                       :order/account
                       :order/paygate-data
                       :order/remote-address
                       :order/temporary-account?
                       {:order/product [:product/service-key]}
                       {:order/product-price [{:product-price/time-frame [:time-frame/days]}]}
                       :sys/status]
                      [:order/paygate-order-ref (:address payment-details)])]
    (when (:db/id order) order)))

(defn handle-btc-received [billing event]
  (let [{:keys [core data-provider logger]} billing
        {:keys [payment-details amount]} event
        conn (api/get-conn data-provider)]
    (if-let [order (find-payment-order (d/db conn) payment-details)]
      (let [{:keys [:order/account :order/product :order/product-price
                    :order/paygate-amount :order/paygate-received-amount
                    :order/temporary-account?]} order
            order-id (:db/id order)
            order-status (:sys/status order)
            required paygate-amount
            recieved (+ (or paygate-received-amount 0) amount)
            new-status (cond
                         (= order-status :order-statuses/confirmed)
                         order-status
                         (>= recieved required)
                         :order-statuses/paid
                         (> recieved 0)
                         :order-statuses/partially-paid
                         :else order-status)]
        (d/transact conn
                    [[:db/add order-id :sys/status new-status]
                     [:db/add order-id :order/paygate-received-amount recieved]
                     [:db/add order-id :order/expiry-date (order-expiry-instant billing)]])
        (when (#{:order-statuses/paid
                 :order-statuses/partially-confirmed
                 :order-statuses/confirmed
                 :order-statuses/forwarded} new-status)
          (provisioning-account-activate
            billing {:account            account
                     :access-level       (:product/service-key product)
                     :days               (get-in product-price
                                                 [:product-price/time-frame :time-frame/days])
                     :temporary-account? temporary-account?}))
        (api/call-async
          core
          {:qn      :transport/send-to!
           :address (read-string (:order/remote-address order))
           :data    {:qn              :billing/payment-received
                     :order-id        order-id
                     :amount-recieved amount
                     :total-received  recieved
                     :order-status    new-status}}))
      (logger :warn "Unknown payment recieved:" (pr-str event)))))

(defn handle-btc-confirmed [billing event]
  (let [{:keys [core data-provider logger]} billing
        {:keys [payment-details amount]} event
        conn (api/get-conn data-provider)]
    (if-let [order (find-payment-order (d/db conn) payment-details)]
      (let [{:keys [:order/paygate-amount
                    :order/paygate-confirmed-amount
                    :order/paygate-data]} order
            paygate-data (if paygate-data (edn/read-string paygate-data) [])
            tx-hash (:tx-hash payment-details)
            confirmed-tx? (some #{tx-hash} paygate-data)
            order-id (:db/id order)
            order-status (:sys/status order)
            required paygate-amount]
        (when (nil? confirmed-tx?)
          (let [confirmed (+ (or paygate-confirmed-amount 0) amount)
                new-status (cond
                             (>= confirmed required)
                             :order-statuses/confirmed
                             (> confirmed 0)
                             :order-statuses/partially-confirmed
                             :else order-status)
                paygate-data (conj paygate-data tx-hash)
                response (when (and (= :order-statuses/confirmed new-status)
                                    (not= order-status new-status))
                           (async/<!!
                             (api/call-async
                               core
                               {:qn        :btc-gate/forward-payment
                                :order-id  (str order-id)
                                :order-ref (:address payment-details)
                                :amount    confirmed})))
                forwarded? (and response (api/success? core response))
                paygate-data (if (and response (not forwarded?))
                               (conj paygate-data [:fwerr (:errors response)])
                               paygate-data)
                new-status (if forwarded? :order-statuses/forwarded new-status)]
            (d/transact
              conn
              (filterv some?
                       [[:db/add order-id :sys/status new-status]
                        (when forwarded?
                          [:db/add order-id :order/paygate-forwarded-amount
                           (get-in response [:result :forwarded-amount])])
                        [:db/add order-id :order/paygate-confirmed-amount confirmed]
                        [:db/add order-id :order/paygate-data
                         (pr-str (conj paygate-data tx-hash))]])))))
      (logger :warn "Unknown payment confirmed:" (pr-str event)))))

(def process-order-schema
  [[:data/type :data-type/map]
   [:map/spec-closed
    {:order/product
     [[:data/requirement :data-requirement/required]
      [:data/alternative
       [[:data/type :data-type/long]]
       [[:data/type :data-type/keyword]]]]
     :order/product-price
     [[:data/requirement :data-requirement/required]
      [:data/type :data-type/long]]
     :order/account-data
     [[:data/type :data-type/map]
      [:map/spec-closed
       {:user/login
                  [[:data/requirement :data-requirement/required]
                   [:data/type :data-type/string]
                   [:string/not-empty]]
        :user/pwd [[:data/requirement :data-requirement/required]
                   [:data/type :data-type/string]
                   [:string/not-empty]]
        :user/nickname
                  [[:data/requirement :data-requirement/required]
                   [:data/type :data-type/string]
                   [:string/not-empty]]}]]
     :order/paygate
     [[:data/requirement :data-requirement/required]
      [:data/alternative
       [[:data/type :data-type/long]]
       [[:data/type :data-type/keyword]]]]}]])

(def prolong-order-schema
  [[:data/type :data-type/map]
   [:map/spec-closed
    {:order/token
     [[:data/requirement :data-requirement/required]
      [:data/type :data-type/string]
      [:string/not-empty]]}]])

(def calculate-order-schema
  [[:data/type :data-type/map]
   [:map/spec-closed
    {:order/product
     [[:data/requirement :data-requirement/required]
      [:data/alternative
       [[:data/type :data-type/long]]
       [[:data/type :data-type/keyword]]]]
     :order/product-price
     [[:data/requirement :data-requirement/required]
      [:data/type :data-type/long]]
     :order/paygate
     [[:data/requirement :data-requirement/required]
      [:data/alternative
       [[:data/type :data-type/long]]
       [[:data/type :data-type/keyword]]]]}]])

(defn get-product-prices [billing]
  (let [{:keys [data-provider]} billing
        db (api/get-db data-provider)]
    (->>
      (d/q '[:find
             (pull ?product
                   [:* {:product/pricing [:* {:product-price/time-frame [:*]}]}])
             :in $
             :where
             [?product :product/service-key]]
           db)
      (mapcat
        (fn [product]
          (let [{:keys [:product/service-key
                        :product/pricing]}
                (first product)]
            (map (fn [price]
                   [[service-key
                     (get-in price [:product-price/time-frame :db/ident])]
                    [(:db/id price) (:product-price/value price)]])
                 pricing))))
      (into {}))))

(defn serve-promo-data [billing request]
  (let [{:keys [core faq-source]} billing]
    (let [faq (when faq-source (api/read-resource faq-source))]
      (api/respond-success
        core request
        {:prices (get-product-prices billing)
         :faq    faq}))))

(defn get-orders [db request & [with-info?]]
  (let [base-currency (d/pull db [:*] :currency/USD)
        currency-associator (fn [items] (mapv (fn [item] (assoc item :base-currency base-currency)) items))
        data
        (db/fetch-data
          db
          (-> request
              (update
                :pattern
                #(or %
                     [:*
                      {:sys/entity [:db/id :db/ident]}
                      {:order/product [:*]}
                      {:order/product-price [:* {:product-price/time-frame [:*]}]}
                      {:order/paygate [:* {:paygate/currency [:*]}]}
                      {:sys/status [:*]}
                      ]))
              (update
                :search-fields
                #(or %
                     [:order/account :order/paygate-order-ref]))
              (assoc :entity :entity.billing/order))
          with-info?)]
    (if with-info?
      (update data :value currency-associator)
      (currency-associator data))))

(defn get-order [db request]
  (first (get-orders db request)))

(defn serve-browse-orders [billing request]
  (let [{:keys [core data-provider]} billing
        db (api/get-db data-provider)]
    (api/respond-success
      core request (get-orders db request {:value-field :value}))))

(defn serve-read-order [billing request]
  (let [{:keys [core data-provider]} billing
        db (api/get-db-filtered data-provider)]
    (api/respond-success
      core request (get-order db request))))

(defn serve-get-balance [billing request]
  )

(defn serve-browse-products [billing request]
  )

(defn get-expired-orders-data [billing]
  (let [{:keys [data-provider]} billing
        db (api/get-db data-provider)]
    (->>
      (d/q '[:find (pull ?order [:db/id :order/account
                                 :order/temporary-account?
                                 :order/remote-address])
             :in $ % ?now-time
             :where
             [?order :order/expiry-date ?expiry]
             [?order :sys/status ?status]
             (expirable-status? ?status)
             [(.getTime ^java.util.Date ?expiry) ?expiry-time]
             [(<= ?expiry-time ?now-time)]]
           db
           '[[(expirable-status? ?status)
              [?status :db/ident :order-statuses/pending]]
             [(expirable-status? ?status)
              [?status :db/ident :order-statuses/partially-paid]]]
           (.getTime (Date.)))
      (map first)
      (mapv order->cancel-info))))

(defn start-order-loop [billing]
  (let [{:keys [logger data-provider]} billing
        loop-control-chan (async/chan)
        timeout (* (api/get-conf billing :await-payment-timeout-minutes 60) 60 1000)
        billing (assoc billing :loop-control-chan loop-control-chan)]
    (async/thread
      (loop []
        (let [[_ port] (async/alts!! [loop-control-chan (async/timeout timeout)])]
          (when-not (= port loop-control-chan)
            (try
              (let [expired-orders (get-expired-orders-data billing)
                    tx (atom [])]
                (doseq [order expired-orders]
                  (when-let [tx-data (process-expired-order billing order)]
                    (swap! tx conj tx-data))
                  (async/<!! (async/timeout 10)))
                (when (seq @tx)
                  (d/transact (api/get-conn data-provider) @tx)))
              (catch Exception e
                (logger :error "Order loop exception:" (.getMessage e))))
            (recur))))
      (logger :info "Order loop closed"))
    billing))

(defn stop-order-loop [billing]
  (when-let [loop-chan (:loop-control-chan billing)]
    (async/close! loop-chan))
  (dissoc billing :loop-control-chan))

(defrecord Billing []
  api/IClientStateProvider
  (provide-client-state [billing _]
    (select-keys (api/get-conf billing) [:await-payment-timeout-minutes]))
  api/IModule
  api/IComponent
  (start [billing]
    (start-order-loop billing))
  (stop [billing]
    (stop-order-loop billing)))

(defn new-billing [& [m]]
  (map->Billing
    (merge
      {api/id-key  :billing
       api/client-state-unit-id-key
                   :promo
       api/static-deps-key
                   [:data-provider]
       api/configuration-key
                   {:base-currency-price-precision 3
                    :await-payment-timeout-minutes 60}
       api/request-servers-key
                   [{:msg-filter      :billing/process-order
                     :handler         #'serve-process-order
                     :validation-opts {:schema process-order-schema}}
                    {:msg-filter      :billing/calculate-order
                     :handler         #'serve-calculate-order
                     :validation-opts {:schema calculate-order-schema}}
                    {:msg-filter      :billing/prolong-order
                     :handler         #'serve-prolong-order
                     :validation-opts {:schema prolong-order-schema}}
                    {:msg-filter      :billing/cancel-order
                     :handler         #'serve-cancel-order
                     :validation-opts {:schema prolong-order-schema}}
                    {:msg-filter      :billing.adm/browse-orders
                     :handler         #'serve-browse-orders
                     :validation-opts {:schema db/search-schema}}
                    {:msg-filter      :billing.adm/read-order
                     :handler         #'serve-read-order
                     :validation-opts {:schema db/read-schema}}
                    {:msg-filter :promo/get-promo-data
                     :handler    #'serve-promo-data}]
       :faq-source nil
       api/event-subs-key
                   [{:msg-filter :btc-gate/payment-received
                     :handler    #'handle-btc-received}
                    {:msg-filter :btc-gate/payment-confirmed
                     :handler    #'handle-btc-confirmed}]}
      m)))