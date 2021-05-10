(ns cryma.app.currency-quotes-provider
  (:require [cryma.core.api :as api]
            [clojure.core.async :as async]
            [clj-http.client :as client]
            [datomic.api :as d])
  (:import (java.math RoundingMode)))

(defprotocol ICurrencyQuoteProvider
  (provide-quotes [provider currencies]))

(defn new-openexchangerates-org-quotes-provider [uri]
  (reify
    ICurrencyQuoteProvider
    (provide-quotes [_ currencies]
      (let [currency-filter (set (map :currency/letter-code currencies))
            currency-code-mapping
            (->> currencies
                 (filter (fn [currency]
                           (currency-filter (:currency/letter-code currency))))
                 (map (fn [currency]
                        [(:currency/letter-code currency) (:db/ident currency)]))
                 (into {}))
            success-statuses #{200 201 202 203 204 205 206 207 300 301 302 303 307}
            result (client/get uri {:as :json-string-keys})
            status (:status result)]

        (if (success-statuses (:status result))
          (->>
            (get-in result [:body "rates"])
            (filter (fn [[k _]] (currency-filter k)))
            (map (fn [[k v]]
                   (let [ident (get currency-code-mapping k)
                         quote (bigdec v)]
                     [ident quote])))
            (into {}))
          [:error status])))))

(defn new-currencylayer-com-quotes-provider [uri]
  (reify
    ICurrencyQuoteProvider
    (provide-quotes [_ currencies]
      (let [currency-filter (set (map :currency/letter-code currencies))
            currency-code-mapping
            (->> currencies
                 (filter (fn [currency]
                           (currency-filter (:currency/letter-code currency))))
                 (map (fn [currency]
                        [(str "USD" (:currency/letter-code currency))
                         (:db/ident currency)]))
                 (into {}))
            success-statuses #{200 201 202 203 204 205 206 207 300 301 302 303 307}
            result (client/get uri {:as :json-string-keys})
            status (:status result)]

        (if (success-statuses (:status result))
          (->>
            (get-in result [:body "quotes"])
            (filter (fn [[k _]] (currency-filter k)))
            (map (fn [[k v]]
                   (let [ident (get currency-code-mapping k)
                         quote (bigdec v)]
                     [ident quote])))
            (into {}))
          [:error status])))))

(defn refresh-quotes [cqp]
  (let [{logger        :logger
         data-provider :data-provider
         currencies    :currencies
         providers     :currency-quotes-providers} cqp]
    (try
      (let [provider-quotes
            (->>
              providers
              (map (fn [provider] (provide-quotes provider currencies)))
              doall
              (filterv map?))
            fst (first provider-quotes)
            fst-ks (keys fst)]
        (->> fst-ks
             (map (fn [k]
                    (let [quotes (->>
                                   provider-quotes
                                   (map (fn [quotes] (get quotes k)))
                                   (filter some?))
                          summ (apply + quotes)
                          cnt (bigdec (count quotes))]
                      [k (.divide summ cnt 12 RoundingMode/HALF_DOWN)])))
             (mapcat (fn [r [k v]]
                       (let [index (- r)]
                         [[:db/add (d/tempid :db.part/billing index)
                           :currency-quote/foreign k]
                          [:db/add (d/tempid :db.part/billing index)
                           :currency-quote/base-to-foreign-quote v]]))
                     (range 1 1000))
             vec
             (d/transact (api/get-conn data-provider))))
      (catch Exception e
        (logger :error "Refresh quotes exception:" (.getMessage e))))))

(defn start-request-quotes-loop [cqp]
  (let [interval (* (api/get-conf cqp :refresh-quotes-interval-minutes) 60 1000)
        control-chan (async/chan)
        logger (:logger cqp)]
    (async/go
      (loop []
        (let [[_ port] (async/alts!! [control-chan (async/timeout interval)])]
          (when (not= port control-chan)
            (logger :debug "Currency quote refresh started")
            (refresh-quotes cqp)
            (recur))))
      (logger :debug "Currency quote loop stopped"))
    (assoc cqp :loop-control-chan control-chan)))

(defn stop-request-quotes-loop [cqp]
  (when-let [ch (:loop-control-chan cqp)]
    (async/close! ch))
  (dissoc cqp :loop-control-chan))

(defn fetch-currencies [data-provider]
  (->>
    (d/q '[:find (pull ?currency [:*])
           :in $
           :where [?currency :currency/letter-code]]
         (api/get-db data-provider))
    (mapv first)))

(defn load-currencies [cqp]
  (let [{:keys [data-provider]} cqp
        currencies (fetch-currencies data-provider)
        cqp (assoc cqp :currencies currencies)]
    (refresh-quotes cqp)
    cqp))

(defrecord CurrencyQuotesProvider []
  api/IModule
  api/IComponent
  (start [cqp]
    (->
      cqp
      (load-currencies)
      (start-request-quotes-loop)))
  (stop [cqp]
    (stop-request-quotes-loop cqp)))

(defn new-currency-quotes-provider [& [m]]
  (map->CurrencyQuotesProvider
    (merge
      {api/id-key          :currency-quotes-provider
       api/static-deps-key [:data-provider]
       api/configuration-key
                           {:refresh-quotes-interval-minutes 1}
       :currency-quotes-providers
                           []}
      m)))

