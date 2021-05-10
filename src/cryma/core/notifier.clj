(ns cryma.core.notifier
  (:require
    [clojure.core.async :as async
     :refer [<! <!! >! >!! put! chan go go-loop]]
    [cryma.core.msg-filters :as mf]
    [cryma.core.tools :as tools]
    [cryma.core.api :as api])
  (:import (java.util Date)))

(def subs-key :notifier/subs)
(def address-limit-key :notifier/adress-limit)

(defn filters->trasducer [filters uuid]
  (let [fns (map mf/msg-filter->fn filters)]
    (comp
      (filter #(some (fn [f] (f %)) fns))
      (map #(assoc % :subscription/uuid uuid)))))

(defn do-unsubscribe [notifier predicate-k-v]
  (let [{:keys [mult subs]} (subs-key notifier)
        to-close (->>
                   @subs
                   (filter predicate-k-v)
                   (map (fn [[k v]] (assoc v :k k))))
        to-close-ks (->> to-close (map :k) set)]
    (doseq [{:keys [channel]} to-close]
      (async/untap mult channel)
      (async/close! channel))
    (swap! subs
           (fn [subs]
             (->>
               subs
               (remove (fn [[k _]] (to-close-ks k)))
               (into {}))))))

(defn apply-address-limit [notifier remote-address result-subs]
  (let [to-unsub (->>
                   result-subs
                   (filter (fn [[_ v]] (= remote-address (:address v))))
                   (sort-by (fn [[_ v]] (:stamp v)) <)
                   (drop-last (address-limit-key notifier))
                   (map first) set)]
    (do-unsubscribe notifier (fn [[k _]] (to-unsub k)))))

(defn do-subscribe [notifier {:keys [remote-address session-id filters] :as params}]
  (assert remote-address "remote-address is required for do-subscribe")
  #_"very strange infinite cycle with this assertions"
  #_(assert session-id "session-id is required for do-subscribe")
  #_(assert (and (vector? filters)
                 (seq filters)) "filters are required for do-subscribe")
  (when-not session-id
    (throw (ex-info "session-id is required for do-subscribe"
                    {:params params})))
  (when-not (and (vector? filters) (seq filters))
    (throw (ex-info "filters is required for do-subscribe"
                    {:filters filters})))
  (let [{:keys [core logger]} notifier
        {:keys [mult subs]} (subs-key notifier)]
    (if-let [uuid (some (fn [[k v]] (when (and (= remote-address (:address v))
                                               (= filters (:filters v)))
                                      k)) @subs)]
      uuid
      (let [uuid (tools/guid)
            transducer (filters->trasducer filters uuid)
            ch (chan 64 transducer
                     (fn [e] (logger :error "notification subscriber chan exceprion: " e)))
            _ (async/tap mult ch)
            result-subs (swap! subs
                               assoc
                               uuid
                               {:channel    ch
                                :address    remote-address
                                :session-id session-id
                                :filters    filters
                                :stamp      (.getTime (Date.))})]
        (apply-address-limit notifier remote-address result-subs)
        (go
          (loop []
            (when-let [notification (<! ch)]
              (let [{:keys [result errors] :as response}
                    (<! (api/call-async
                          core
                          {:qn      :transport/send-to!
                           :address remote-address
                           :data    notification}))]
                (if (api/success? core response)
                  (logger :debug "Notification's sending success result: " result)
                  (logger :error "Notification's sending errors: " errors)))
              (recur)))
          (logger :debug "Subscription channel closed for remote-address: " remote-address))
        uuid))))

(defn serve-subscribe [notifier request]
  (let [core (:core notifier)
        {:keys [remote-address remote-address-components]} (meta request)
        filters (:filters request)]
    (when-not remote-address
      (throw (ex-info "Missing remote-address for subscribtion"
                      {:request request})))
    (let [uuid (do-subscribe
                 notifier
                 {:filters        filters
                  :remote-address remote-address
                  :session-id     (:session-id remote-address-components)})]
      (api/respond-success core request {:subscription/uuid uuid}))))

(defn serve-resubscribe [notifier request]
  (when-not (:remote-address (meta request))
    (throw (ex-info "Missing remote-address for Resubscribtion"
                    {:request request})))
  (let [logger (:logger notifier)
        {:keys [remote-address remote-address-components]} (meta request)
        subscriptions (:subscriptions request)
        subs @(get-in notifier [subs-key :subs])
        result (->>
                 subscriptions
                 (map
                   (fn [[old-k v]]
                     (let [old (get subs old-k)
                           old-address (:address old)]
                       (when (and old-address
                                  (not= remote-address old-address))
                         (logger :warn "Same sub-uuid but different address"
                                 " new address=" remote-address
                                 " old address=" old-address))
                       (when-not old
                         [old-k (do-subscribe
                                  notifier
                                  {:filters        (:filters v)
                                   :remote-address remote-address
                                   :session-id     (:session-id
                                                     remote-address-components)})]))))
                 (filterv some?))]
    (api/respond-success request {:old-new-uuids result})))

(defn handle-unsubscribe [notifier event]
  (let [logger (:logger notifier)
        remote-address (:remote-address (meta event))
        uuid (:subscription/uuid event)]
    (logger :debug "handle unsub remote-address: " remote-address " uuid:" uuid)
    (do-unsubscribe
      notifier
      (fn [[k v]]
        (and (= k uuid)
             (= remote-address (:address v)))))))

(defn handle-connection-closed [notifier event]
  (let [remote-address (:remote-address (meta event))]
    (do-unsubscribe notifier (fn [[_ v]] (= remote-address (:address v))))))

(defn handle-session-dropped [notifier event]
  (let [session-id (get-in event [:session :session-id])]
    (do-unsubscribe notifier
                    (fn [[_ v]] (= session-id (:session-id v))))))

(defrecord Notifier []
  api/IModule
  api/IComponent
  (start [notifier]
    (let [core (:core notifier)
          hub (chan 256)
          mult (async/mult hub)
          notifier (assoc notifier subs-key
                                   {:hub  hub
                                    :mult mult
                                    :subs (atom {})})]
      (api/subscribe-event
        core
        {:msg-filter {:target #{:notifier/subscribers}}
         :module     notifier
         :channel    hub})
      notifier))
  (stop [notifier]
    (when-let [{:keys [hub mult subs]} (subs-key notifier)]
      (when mult (async/untap-all mult))
      (when hub (async/close! hub))
      (do-unsubscribe notifier (fn [_] true)))
    (dissoc notifier subs-key)))

(def dtl-rule-schema
  [[:data/type :data-type/vector]
   [:seq/count {:min 2 :max 2}]
   [:seq/destruct
    [[:data/requirement :data-requirement/required]]
    [[:data/requirement :data-requirement/required]
     [:data/type :data-type/vector]
     [:seq/not-empty]]]])

(def dtl-rules-schema
  [[:data/type :data-type/vector]
   [:seq/not-empty]
   [:seq/each [[:data/alternative
                [[:data/enum :dtlrules]]
                dtl-rule-schema
                [[:data/type :data-type/map]]
                ]]]])

(def dtl-schema
  [[:data/alternative
    dtl-rules-schema
    dtl-rule-schema]])

(def subscribe-schema
  [[:map/spec
    {:filters
     [[:data/requirement :data-requirement/required]
      [:data/type :data-type/vector]
      [:seq/not-empty]
      [:seq/each [[:data/alternative
                   [[:data/type :data-type/map]
                    [:map/spec {:qn [[:data/requirement :data-requirement/required]
                                     [:data/type :data-type/keyword]]}]]
                   [[:data/type :data-type/keyword]]
                   dtl-schema]]]]}]])

(defn new-notifier [& [m]]
  (map->Notifier
    (merge
      {api/id-key         :notifier
       api/request-servers-key
                          [{:msg-filter      :notifier/subscribe
                            :handler         serve-subscribe
                            :validation-opts {:schema subscribe-schema}}
                           {:msg-filter      :notifier/resubscribe
                            :handler         serve-resubscribe
                            :validation-opts {:schema [[:map/spec
                                                        {:subscriptions
                                                         [[:data/type :data-type/map]
                                                          [:seq/not-empty]
                                                          [:map/each [[[:data/type :data-type/uuid]]
                                                                      subscribe-schema]]]}]]}}]

       api/event-subs-key [{:msg-filter :notifier/unsubscribe
                            :handler    handle-unsubscribe}
                           {:msg-filter :transport/connection-closed
                            :handler    handle-connection-closed}
                           {:msg-filter :session/dropped
                            :handler    handle-session-dropped}]
       address-limit-key  20}

      m)))

