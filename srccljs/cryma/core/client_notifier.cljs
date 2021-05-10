(ns cryma.core.client-notifier
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cryma.core.api :as api]
    [cljs.core.async :as async :refer [<! put! >! alts!]]))

(def subs-key :notifier/subs)

(defn has-filters? [subs filters]
  (first
    (filter
      (fn [[_ v]] (= (:filters v) filters)) subs)))

(defn remove-cons [notifier uuid ch]
  (let [core (:core notifier)
        subs (get-in notifier [subs-key :subs])
        sub (-> subs
                (swap! (fn [subs]
                         (if-let [sub (get subs uuid)]
                           (if (> (count (:consumers sub)) 1)
                             (update-in
                               subs [uuid :consumers]
                               (fn [consumers]
                                 (vec (remove (fn [[c _]] (= c ch)) consumers))))
                             (dissoc subs uuid))
                           subs)))
                (get uuid))]
    (async/close! ch)
    (when (and (not sub) @uuid)
      (api/pub-event
        core
        {:qn                :notifier/unsubscribe
         :subscription/uuid @uuid
         :target            :server}))))

(defn start-chan-watcher [notifier uuid ch unsub-ch]
  (go
    (put! ch (api/success-response (:core notifier) {:unsub-ch unsub-ch}))
    (<! unsub-ch)
    (remove-cons notifier uuid ch)
    (async/close! unsub-ch)))

(defn serve-subscribe [notifier request]
  (let [core (:core notifier)
        subs (get-in notifier [subs-key :subs])
        ch (api/get-response-chan core request)
        filters (:filters request)]
    (assert (and (vector? filters) (seq filters))
            "filters are required in serve-subscription")
    (let [new-uuid (atom nil)
          unsub-ch (async/chan 1)
          subs-upd (swap! subs
                          (fn [subs]
                            (if-let [[k _] (has-filters? subs filters)]
                              (update-in subs [k :consumers]
                                         #(conj % [ch unsub-ch]))
                              (assoc subs
                                new-uuid
                                {:filters   filters
                                 :consumers [[ch unsub-ch]]}))))
          uuid (->>
                 subs-upd
                 (mapcat
                   (fn [[uuid sub]]
                     (map (fn [con]
                            {:uuid uuid :ch con})
                          (:consumers sub))))
                 (filter #(= (first (:ch %)) ch))
                 first :uuid)]
      (start-chan-watcher notifier uuid ch unsub-ch)
      (when (= uuid new-uuid)
        (go
          (let [response
                (<! (api/call-async
                      core
                      {:qn      :notifier/subscribe
                       :filters filters
                       :target  :server}))]
            (if (api/success? core response)
              (reset! new-uuid (get-in response [:result :subscription/uuid]))
              (when-let [v (get @subs new-uuid)]
                (swap! subs dissoc new-uuid)
                (doseq [[ch subch] (:consumers v)]
                  (put! ch response)
                  (async/close! subch))))))))))

(defn handle-notification-event [notifier event]
  (let [{:keys [core logger]} notifier
        uuid (:subscription/uuid event)
        event (dissoc event :subscription/uuid)]
    (logger :debug "notif-event" uuid event)
    (if-let [[_ sub] (and uuid
                          (->>
                            @(get-in notifier [subs-key :subs])
                            (filter (fn [[k _]] (= @k uuid)))
                            first))]
      (doseq [[_ sch _]
              (doall
                (->>
                  (:consumers sub)
                  (map (fn [[ch sch]] [ch sch (put! ch event)]))
                  (filter (fn [[_ _ opened?]] (not opened?)))))]
        (async/close! sch))
      (when uuid
        (api/pub-event
          core
          {:qn                :notifier/unsubscribe
           :subscription/uuid uuid
           :target            :server})))))

(defn handle-connection-opened [notifier event]
  (when-not (:first-open? event)
    (let [{:keys [core logger]} notifier
          subs (get-in notifier [subs-key :subs])]
      (when-not (empty? @subs)
        (go
          (let [response
                (<! (api/call-async
                      core
                      {:qn            :notifier/resubscribe
                       :subscriptions (->>
                                        @subs
                                        (remove (fn [[k _]] (nil? @k)))
                                        (map (fn [[k sub]]
                                               [@k (dissoc sub :consumers)]))
                                        (into {}))
                       :target        :server}))]
            (if (api/success? core response)
              (let [old-new-uuids (get-in response [:result :old-new-uuids])
                    old-new-uuids (into {} old-new-uuids)]
                (doall
                  (map (fn [[k _]]
                         (when-let [new (get @k old-new-uuids)]
                           (reset! k new))) @subs)))
              (logger :error "failed to resubscribe notifications with result" response))))))))

(defrecord Notifier []
  api/IModule
  api/IComponent
  (start [notifier]
    (assoc notifier subs-key {:subs (atom {})}))
  (stop [notifier]
    (dissoc notifier subs-key)))

(defn new-notifier [& [m]]
  (map->Notifier
    (merge
      {api/id-key         :notifier
       api/request-servers-key
                          [{:msg-filter {:qn     :notifier/subscribe
                                         :target #{:client nil}}
                            :handler    serve-subscribe}]

       api/event-subs-key [{:msg-filter :transport/connection-opened
                            :handler    handle-connection-opened}
                           {:msg-filter {:target #{:notifier/subscribers}}
                            :handler    handle-notification-event}]}

      m)))