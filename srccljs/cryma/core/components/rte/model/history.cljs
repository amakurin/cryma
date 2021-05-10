(ns cryma.core.components.rte.model.history
  (:require
    [cryma.core.components.rte.model.configuration :as configuration]))

;;=============================
;; HISTORY
;;=============================

(defn current-timestamp []
  (.getTime (js/Date.)))

(defn get-ms-before [timestamp]
  (- (current-timestamp) timestamp))

(defn not-so-far? [timestamp boundary]
  (< (get-ms-before timestamp) boundary))

(defn sliding-conj [v max-len value]
  (conj (vec (take-last (dec max-len) v)) value))

(defn append-past
  [{:keys [past] :as history}
   {:keys [join-interval-ms join-actions-fn history-conf]}
   action state]
  (let [last-entry (last past)]
    (if (and last-entry
             (= (get-in last-entry [:action :qn]) (:qn action))
             join-interval-ms join-actions-fn
             (not-so-far? (:timestamp last-entry) join-interval-ms))
      (update history :past
              (fn [past]
                (conj (pop past)
                      (-> last-entry
                          (assoc :timestamp (current-timestamp))
                          (update :action join-actions-fn action)))))
      (update history :past
              sliding-conj (or (:store-n-last-entries history-conf) 20)
              {:state     state
               :timestamp (current-timestamp)
               :action    action}))))

(defmulti do-history-operation (fn [_ hist-op _ _] (:qn hist-op)))

(defmethod do-history-operation :doc.history.api/append
  [current-state history-op action state]
  (update
    current-state :history
    (fn [h]
      (-> h
          (append-past history-op action state)
          (assoc :future [])))))

(defmethod do-history-operation :doc.history.api/undo
  [current-state _ _ state]
  (if-let [last-entry (-> current-state :history :past peek)]
    (let [{:keys [doc selection]} (:state last-entry)]
      (->
        current-state
        (assoc :doc doc)
        (assoc :selection selection)
        (update
          :history
          (fn [history]
            (-> history
                (update :past pop)
                (update :future conj
                        (assoc last-entry :state state)))))))
    current-state))

(defmethod do-history-operation :doc.history.api/redo
  [current-state _ _ state]
  (if-let [last-entry (-> current-state :history :future peek)]
    (let [{:keys [doc selection]} (:state last-entry)]
      (->
        current-state
        (assoc :doc doc)
        (assoc :selection selection)
        (update
          :history
          (fn [history]
            (-> history
                (update :future pop)
                (update :past conj
                        (assoc last-entry :state state)))))))
    current-state))

(defmethod do-history-operation :default
  [history _ _ _] history)

(defn update-history [current-state action state]
  (let [conf (configuration/get-conf (:doc current-state))]
    (if-let [history-op (get-in conf [:api (:qn action) :history-operation])]
      (do-history-operation
        current-state (assoc history-op :history-conf (:history conf))
        action (dissoc state :history))
      current-state)))

(defn init-history []
  {:past [] :future []})

(defn history-join-type-in [last-action action]
  (update last-action :key str (:key action)))

(defn history-join-counter [last-action _]
  (update last-action :count (fn [c] (inc (or c 1)))))

(defn history-operation
  ([operation]
   {:history-operation {:qn operation}})
  ([operation join-interval-ms join-fn]
   {:history-operation
    {:qn               operation
     :join-interval-ms join-interval-ms
     :join-actions-fn  join-fn}}))

