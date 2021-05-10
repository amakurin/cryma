(ns cryma.core.components.channels
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [cljs.core.async :as async :refer [alts! <!]]))

(defn channel-props [_ _ _])

(defn ch4read [state]
  (get-in state [:sys.channels/chans :ch4read]))

(defn ch4write [state]
  (get-in state [:sys.channels/chans :ch4write]))

(defn alts!ev [ch ev]
  (go
    (let
      [[_ port] (alts! [[ch ev] (async/timeout 1000)])]
      (when-not (= port ch)
        (warn "Nobody reads ev: " ev)))))

(defn alts!stev [state ev]
  (alts!ev (ch4write state) ev))

(defn deserialize [sys-chans]
  (when sys-chans
    (->> sys-chans
         js->clj
         (map
           (fn [[k v]]
             (let [k (case k
                       "api" :sys.channels/api
                       "root-ch" :sys.channels/root-ch
                       "chans" :sys.channels/chans
                       (keyword k))]
               [k (if (map? v) (deserialize v) v)])))
         (into {}))))

(defn channels [& [new-root-channel?]]
  {:will-mount
   (fn [state]
     (let [sys-chans
           (when-not new-root-channel?
             (deserialize
               (aget (aget (:rum/react-component state) "context") "sysChannels")))]
       (if sys-chans
         (merge state sys-chans)
         (let [root-ch (async/chan (async/sliding-buffer 1000))
               control-mult (async/mult root-ch)
               make-ch4read (fn [& [transducer]]
                              (let [ch (if transducer
                                         (async/chan 1 transducer)
                                         (async/chan 1))]
                                (async/tap control-mult ch)))
               make-ch4write (fn [& [transducer]]
                               (let [ch (if transducer
                                          (async/chan 1 transducer)
                                          (async/chan 1))]
                                 (async/pipe ch root-ch false)
                                 ch))]
           (->
             state
             (assoc :sys.channels/root-ch root-ch)
             (assoc :sys.channels/api
                    {:make-ch4read   make-ch4read
                     :make-ch4write  make-ch4write
                     :close-ch4read  (fn [ch]
                                       (async/untap control-mult ch)
                                       (async/close! ch))
                     :close-ch4write (fn [ch]
                                       (async/close! ch))}))))))
   :transfer-state
   (fn [old new]
     (merge new
            (select-keys
              old
              [:sys.channels/api :sys.channels/root-ch :sys.channels/chans])))
   :child-context
   (fn [state]
     {:sysChannels
      (select-keys
        state
        [:sys.channels/api])})

   :class-properties
   {:contextTypes
    {:sysChannels channel-props}
    :childContextTypes
    {:sysChannels channel-props}}

   :will-unmount
   (fn [state]
     (go
       (when-let [{:keys [ch4read ch4write]}
                  (:sys.channels/chans state)]
         (let [{:keys [close-ch4read close-ch4write]}
               (:sys.channels/api state)]
           (when ch4write
             (async/put! ch4write {:qn :sys.channels/closing})
             (<! (async/timeout 200)))
           (close-ch4read ch4read)
           (close-ch4write ch4write)))
       (when-let [root-ch (:sys.channels/root-ch state)]
         (async/put! root-ch {:qn :sys.channels/root-closing})
         (<! (async/timeout 200))
         (async/close! root-ch)))
     state)
   })

(defn simple-chans [event-handler]
  {:will-mount
   (fn [state]
     (if-let [{:keys [make-ch4read make-ch4write]}
              (:sys.channels/api state)]
       (let [ch4read (make-ch4read (map identity))
             ch4write (make-ch4write (map identity))
             state (-> state
                       (assoc :sys.channels/chans
                              {:ch4read ch4read :ch4write ch4write}))]
         (go
           (loop []
             (when-let [ev (<! ch4read)]
               (when event-handler
                 (event-handler state ev))
               (recur)))
           #_(debug "read loop exit rum/id" (:rum/id state)))
         state)
       state))})

