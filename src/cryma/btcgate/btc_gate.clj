(ns cryma.btcgate.btc-gate
  (:require
    [clojure.core.async :as async]
    [cryma.core.api :as api]
    [datomic.api :as d])
  (:import
    (java.io File)
    (org.bitcoinj.core
      ECKey
      DumpedPrivateKey
      Address Coin Transaction Context)
    (org.bitcoinj.wallet Wallet)
    (org.bitcoinj.params TestNet3Params MainNetParams)
    (org.slf4j LoggerFactory)
    (org.bitcoinj.kits WalletAppKit)
    (org.bitcoinj.wallet.listeners AbstractWalletEventListener)
    (org.bitcoinj.wallet Wallet$BalanceType)))

(defn prod-net []
  (MainNetParams.))

(defn test-net []
  (TestNet3Params.))

(defn create-keypair []
  (ECKey.))

(defn ->private
  [key-pair network-params]
  (str (.getPrivateKeyEncoded key-pair network-params)))

(defn ->base58-btc-address
  [key-pair network-params]
  (str (.toAddress key-pair network-params)))

(defn ->key-pair
  [s network-params]
  (.getKey (DumpedPrivateKey. network-params s)))

(defn key-pair-equals?
  [a b] (= 0 (.compare ECKey/PUBKEY_COMPARATOR a b)))

;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;:::::::::::::::::::::::::::::::::::: TESTS ::::::::::::::::::::::::::::::::::::::::::::::::
;;:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

#_(def kp (create-keypair))

#_(def priv (->private kp))

#_(key-pair-equals? kp (->key-pair priv))

#_(def kpp (->key-pair private-test))

#_(def kpp2 (->key-pair private-test2))

#_(def addr2 (.toAddress kpp2 net))

#_(.importKey wallet kpp2)

#_(def addr (.toAddress kpp net))

#_(str addr)

(defn btc-network-params [network]
  (case network
    :btc-network/test (test-net)
    :btc-network/prod (prod-net)
    (test-net)))

(defn make-wallet-name [btc-gate]
  (let [{:keys [network wallet-name]} (api/get-conf btc-gate)
        wallet-prefix (name (or network :unknown))]
    (str wallet-prefix "-" wallet-name)))

(defn logback-level [level-key]
  (case level-key
    :trace ch.qos.logback.classic.Level/TRACE
    :debug ch.qos.logback.classic.Level/DEBUG
    :info ch.qos.logback.classic.Level/INFO
    :warn ch.qos.logback.classic.Level/WARN
    :error ch.qos.logback.classic.Level/ERROR
    ch.qos.logback.classic.Level/OFF))

(defn set-wallet-log-level [level]
  (let [logger (LoggerFactory/getLogger "org.bitcoinj")]
    (.setLevel logger (logback-level level))))

(defn coin->bigdec [coin]
  (BigDecimal. (.toPlainString coin)))

(defn bigdec->coin [bigdec-value]
  (Coin/parseCoin (.toPlainString bigdec-value)))

(defn my-tx-outputs [tx wallet]
  (let [network-params (.getParams wallet)]
    (mapv (fn [tx-output]
            (let [p2pkh-address (.getAddressFromP2PKHScript tx-output
                                                            network-params)
                  p2sh-address (.getAddressFromP2SH tx-output
                                                    network-params)]
              {:address (str (or p2pkh-address p2sh-address))
               :amount  (coin->bigdec (.getValue tx-output))}))
          (.getWalletOutputs tx wallet))))

(defn coins-received-listener [btc-gate]
  (let [control-chan (:btc-control-chan btc-gate)
        {:keys [btc-confidence-level btc-confidence-level-limit]} (api/get-conf btc-gate)]
    (proxy [AbstractWalletEventListener] []
      (onCoinsReceived [^Wallet wallet
                        ^Transaction tx
                        ^Coin prev-balance
                        ^Coin new-balance]
        (async/put! control-chan
                    {:qn          :btc-kit/coins-received
                     :tx-hash     (.getHashAsString tx)
                     :prev-balace (coin->bigdec prev-balance)
                     :new-balance (coin->bigdec new-balance)
                     :my-outputs  (my-tx-outputs tx wallet)}))
      (onTransactionConfidenceChanged [^Wallet wallet
                                       ^Transaction tx]
        (let [depth (.getDepthInBlocks (.getConfidence tx))]
          (when (and (>= depth btc-confidence-level)
                     (< depth btc-confidence-level-limit))
            (async/put! control-chan
                        {:qn         :btc-kit/tx-confirmed
                         :tx-hash    (.getHashAsString tx)
                         :depth      depth
                         :my-outputs (my-tx-outputs tx wallet)})))))))

(defn setup-btc-kit [network-params-or-context file-path wallet-name control-channel]
  (proxy
    [WalletAppKit] [network-params-or-context (File. file-path) wallet-name]
    (onSetupCompleted []
      (async/put! control-channel {:qn :btc-kit/setup}))))

(defn get-wallet [btc-gate]
  (when-let [kit (:btc-kit btc-gate)]
    (.wallet kit)))

(defn get-wallet-balance [btc-gate & [balance-type-key]]
  (let [wallet (get-wallet btc-gate)
        balance-type (case balance-type-key
                       :balance-types/available
                       Wallet$BalanceType/AVAILABLE
                       :balance-types/estimated
                       Wallet$BalanceType/ESTIMATED
                       :balance-types/estimated-spendable
                       Wallet$BalanceType/ESTIMATED_SPENDABLE
                       Wallet$BalanceType/AVAILABLE_SPENDABLE)]
    (.getBalance wallet balance-type)))

(defn get-peer-group [btc-gate]
  (when-let [kit (:btc-kit btc-gate)]
    (.peerGroup kit)))

(defn get-network-params [btc-gate]
  (when-let [context (:btc-kit-context btc-gate)]
    (.getParams context)))

(defn update-forwarding-addresses [btc-gate]
  (let [{:keys [btc-forwarding-addresses data-provider]} btc-gate
        db (api/get-db data-provider)
        addrs (mapv first (d/q '[:find ?addr
                                 :in $ ?network
                                 :where
                                 [?e :forwarding/network ?network]
                                 [?e :forwarding/address ?addr]]
                               db (api/get-conf btc-gate :network)))]
    (reset! btc-forwarding-addresses addrs)))

(defn get-forwarding-addrs [btc-gate]
  @(:btc-forwarding-addresses btc-gate))

(defn get-forwarding-addr [btc-gate]
  (let [addrs (get-forwarding-addrs btc-gate)
        addr (get addrs (rand-int (count addrs)))]
    (Address/fromBase58 (get-network-params btc-gate) addr)))

(defn send-coins [btc-gate btc-address btc-amount]
  (let [wallet (get-wallet btc-gate)
        peer-group (get-peer-group btc-gate)
        network-params (get-network-params btc-gate)
        address (Address/fromBase58 network-params btc-address)
        amount (Coin/parseCoin (str btc-amount))]
    (.sendCoins wallet peer-group address amount)))

(defmulti handle-btc-event (fn [_ btc-event] (:qn btc-event)))

(defmethod handle-btc-event :btc-kit/setup
  [btc-gate _]
  (let [{:keys [btc-kit logger]} btc-gate
        wallet (.wallet btc-kit)]
    (logger :info "BTC Wallet starting...")
    (.addEventListener wallet (coins-received-listener btc-gate))))

(defmethod handle-btc-event :btc-kit/started
  [btc-gate _]
  (let [{:keys [logger]} btc-gate]
    (logger :info "BTC Wallet started")))

(defn recieved->core-events [db tx-data qn]
  (let [{:keys [tx-hash my-outputs]} tx-data
        tx-data (mapv (fn [output] (assoc output :tx-hash tx-hash)) my-outputs)
        prs (d/pull-many
              db [:payment-request/address :payment-request/order-id
                  :btc/amount :sys/status]
              (mapv (fn [output] [:payment-request/address (:address output)]) tx-data))
        addr->pr-mapping (->> prs
                              (map (fn [pr]
                                     (when-let [address (:payment-request/address pr)]
                                       [address pr])))
                              (into {}))]
    (->> tx-data
         (map (fn [{:keys [address amount tx-hash]}]
                (when-let [pr (get addr->pr-mapping address)]
                  {:qn              qn
                   :order-id        (:payment-request/order-id pr)
                   :amount          amount
                   :payment-details {:address address
                                     :tx-hash tx-hash}})))
         (filterv some?))))

(defmethod handle-btc-event :btc-kit/coins-received
  [btc-gate btc-event]
  (let [{:keys [core data-provider]} btc-gate
        db (api/get-db data-provider)
        events (recieved->core-events db btc-event :btc-gate/payment-received)]
    (when (seq events)
      (api/pub-events core events))))

(defmethod handle-btc-event :btc-kit/tx-confirmed
  [btc-gate btc-event]
  (let [{:keys [core data-provider logger]} btc-gate
        db (api/get-db data-provider)
        events (recieved->core-events db btc-event :btc-gate/payment-confirmed)]
    (logger :debug "CONFIRMED EVENT: " btc-event)
    (when (seq events)
      (api/pub-events core events))))

(defmethod handle-btc-event :default
  [btc-gate btc-event]
  ((:logger btc-gate) :debug "Unknown btc-kit-event qn:" (:qn btc-event)))

(defn start-btc-event-loop [btc-gate]
  (let [{:keys [btc-control-chan btc-kit btc-kit-context logger]} btc-gate]
    (async/go
      (loop []
        (when-let [btc-event (async/<! btc-control-chan)]
          (Context/propagate btc-kit-context)
          (logger :debug "Btc event qn:" (:qn btc-event))
          (try
            (handle-btc-event btc-gate btc-event)
            (catch Exception e
              (logger :error (.getMessage e))))
          (recur)))
      (Context/propagate btc-kit-context)
      (.shutDown btc-kit)
      (logger :info "BTC-KIT SHUTDOWN"))))

(defn start-wallet [btc-gate]
  (let [{:keys [network wallet-path wallet-log-level]} (api/get-conf btc-gate)
        btc-control-chan (async/chan 1)
        btc-kit-context (Context. (btc-network-params network))
        btc-kit (setup-btc-kit
                  btc-kit-context
                  wallet-path
                  (make-wallet-name btc-gate)
                  btc-control-chan)
        btc-gate (assoc btc-gate
                   :btc-kit btc-kit
                   :btc-kit-context btc-kit-context
                   :btc-control-chan btc-control-chan
                   :btc-forwarding-addresses (atom []))]
    (update-forwarding-addresses btc-gate)
    (set-wallet-log-level wallet-log-level)
    (start-btc-event-loop btc-gate)
    (.startAsync btc-kit)
    (.awaitRunning btc-kit)
    (async/put! btc-control-chan {:qn :btc-kit/started})
    btc-gate))

(defn stop-wallet [btc-gate]
  (let [{:keys [btc-control-chan]} btc-gate]
    (when btc-control-chan
      (async/close! btc-control-chan))
    (dissoc btc-gate :btc-control-chan :btc-kit)))

(defn add-btc-address [btc-gate]
  (let [wallet (get-wallet btc-gate)
        network-params (get-network-params btc-gate)
        key-pair (.freshReceiveKey wallet)
        private (->private key-pair network-params)
        btc-addr (->base58-btc-address key-pair network-params)]
    {:private private :address btc-addr}))

(defn create-payment-request [data-provider payment-request]
  (let [conn (api/get-conn data-provider)
        temp-id (d/tempid :db.part/btc-gate)
        pr (assoc payment-request :db/id temp-id)
        tx-result @(d/transact conn [pr])]
    (assoc pr
      :db/id (d/resolve-tempid (:db-after tx-result) (:tempids tx-result) temp-id))))

(defn serve-request-payment [btc-gate request]
  (let [{:keys [order-id amount]} request
        {:keys [data-provider core]} btc-gate
        {:keys [private address]} (add-btc-address btc-gate)
        _
        (create-payment-request
          data-provider {:payment-request/order-id order-id
                         :payment-request/address  address
                         :payment-request/private  private
                         :btc/amount               amount})]
    (api/respond-success
      core request {:payment-details {:address address :amount amount}})))

(defn serve-forward-payment [btc-gate request]
  (let [{:keys [core data-provider btc-kit-context logger]} btc-gate
        {:keys [order-id order-ref amount]} request
        _ (Context/propagate btc-kit-context)
        db (api/get-db data-provider)
        pay-request (d/pull db [:*] [:payment-request/address order-ref])
        balance (get-wallet-balance btc-gate)
        amount (bigdec->coin amount)
        amount-to-send (.subtract amount
                                  Transaction/DEFAULT_TX_FEE)]
    (try
      (when (.isLessThan balance amount)
        (throw (ex-info "Insufficient balance"
                        {:code    :insufficient-balance
                         :field   :amount
                         :request request
                         :balance (coin->bigdec balance)})))
      (when (not= order-id (:payment-request/order-id pay-request))
        (throw (ex-info "Wrong order-ref in forward request"
                        {:code    :wrong-order-ref
                         :field   :order-ref
                         :request request})))
      (.sendCoins
        (get-wallet btc-gate)
        (get-peer-group btc-gate)
        (get-forwarding-addr btc-gate)
        amount-to-send)
      (api/respond-success
        core request
        {:forwarded-amount (coin->bigdec amount-to-send)})

      (catch Exception e
        (let [data (ex-data e)
              ex-message (.getMessage e)]
          (if (:code data)
            (api/respond-error core request data)
            (do
              (logger :error "Forward payment exception:" ex-message)
              (api/respond-exception core request :internal-exception
                                     [:qn :order-id :order-ref]
                                     {:exception-message ex-message}))))))))

(defn get-full-balance [btc-gate]
  {:spendable (coin->bigdec (get-wallet-balance btc-gate))
   :available (coin->bigdec (get-wallet-balance btc-gate :balance-types/available))})

(defn serve-get-balance [btc-gate request]
  (let [result (get-full-balance btc-gate)]
    (api/respond-success
      (:core btc-gate) request {:network (api/get-conf btc-gate :network) :balance result})))

(defn serve-force-forwarding [btc-gate request]
  (let [{:keys [core btc-kit-context logger]} btc-gate
        _ (Context/propagate btc-kit-context)
        balance (get-wallet-balance btc-gate)
        amount-to-send (.subtract balance
                                  Transaction/DEFAULT_TX_FEE)]
    (try
      (when (.isPositive amount-to-send)
        (.sendCoins
          (get-wallet btc-gate)
          (get-peer-group btc-gate)
          (get-forwarding-addr btc-gate)
          amount-to-send))
      (api/respond-success
        core request
        {:balance          (get-full-balance btc-gate)
         :forwarded-amount (if (.isPositive amount-to-send) (coin->bigdec amount-to-send) 0)})

      (catch Exception e
        (let [data (ex-data e)
              ex-message (.getMessage e)]
          (if (:code data)
            (api/respond-error core request data)
            (do
              (logger :error "Forward payment exception:" ex-message)
              (api/respond-exception core request :internal-exception
                                     [:qn :order-id :order-ref]
                                     {:exception-message ex-message}))))))))

(defrecord BtcGate []
  api/IModule
  api/IComponent
  (start [btc-gate]
    (start-wallet btc-gate))
  (stop [btc-gate]
    (stop-wallet btc-gate)))

(def request-payment-schema
  [[:data/type :data-type/map]
   [:map/spec-closed
    {:order-id [[:data/requirement :data-requirement/required]
                [:data/type :data-type/string]
                [:string/not-empty]]
     :amount   [[:data/requirement :data-requirement/required]
                [:data/type :data-type/decimal]
                [:number/range {:min 0.00000001M
                                :max 21000000M}]]}]])

(def forward-payment-schema
  [[:data/type :data-type/map]
   [:map/spec-closed
    {:order-id  [[:data/requirement :data-requirement/required]
                 [:data/type :data-type/string]
                 [:string/not-empty]]
     :order-ref [[:data/requirement :data-requirement/required]
                 [:data/type :data-type/string]
                 [:string/not-empty]]
     :amount    [[:data/requirement :data-requirement/required]
                 [:data/type :data-type/decimal]
                 [:number/range {:min 0.00000001M
                                 :max 21000000M}]]}]])

(defn new-btc-gate [& [m]]
  (map->BtcGate
    (merge
      {api/id-key
       :btc-gate
       api/static-deps-key
       [:data-provider]
       api/configuration-key
       {:network                    :btc-network/test
        :wallet-name                "wallet"
        :wallet-path                "."
        :wallet-log-level           :error
        :btc-confidence-level       1
        :btc-confidence-level-limit 7}
       api/request-servers-key
       [{:msg-filter      :btc-gate/request-payment
         :handler         #'serve-request-payment
         :validation-opts {:schema request-payment-schema}}
        {:msg-filter      :btc-gate/forward-payment
         :handler         #'serve-forward-payment
         :validation-opts {:schema forward-payment-schema}}
        {:msg-filter :btc-gate/get-balance
         :handler    #'serve-get-balance}
        {:msg-filter :btc-gate/force-forwarding
         :handler    #'serve-force-forwarding}]}
      m)))


