(ns cryma.core.spa-router
  (:require [cryma.core.api :as api]))

(defrecord SpaRouter []
  api/IModule
  api/IComponent
  (start [spa-router]
    (assoc spa-router :history (atom [])))
  (stop [spa-router]
    spa-router))

(defn conj-history [history event]
  (vec (take-last 3 (conj history event))))

(defn spa-handler [spa-router route-spec component event]
  (let [spa (:spa spa-router)
        {:keys [route-component route-merge-data]} route-spec
        spa-event {:route-component route-component
                   :component       component
                   :event           (merge event route-merge-data)}]
    (swap! (:history spa-router) conj-history spa-event)
    (api/change-route spa spa-event)))

(defn register-spa-routes [spa-router units]
  (let [core (:core spa-router)
        route-specs (->> units
                         (mapcat (fn [unit]
                                   (map (fn [spec]
                                          (when (:route-component spec)
                                            (assoc spec :module unit)))
                                        (api/routes-key unit))))
                         (remove nil?))]
    (doseq [spec route-specs]
      (api/subscribe-event
        core
        (-> spec
            (dissoc :route-component)
            (assoc :handler (partial spa-handler spa-router
                                     (select-keys spec [:route-component :route-merge-data]))))))
    spa-router))

(defn handle-prev [spa-router _]
  (if-let [{:keys [route-component component event]} (last (butlast @(:history spa-router)))]
    (spa-handler spa-router {:route-component route-component} component event)
    (when-let [event (:default-navigation-prev-event spa-router)]
      (api/pub-event (:core spa-router) event))))

(defn deps-pred [unit]
  (when-let [routes (api/routes-key unit)]
    (some :route-component routes)))

(defn new-spa-router [& [m]]
  (map->SpaRouter
    (merge
      {api/id-key          :spa-router
       api/static-deps-key [:spa :core]
       api/event-subs-key  [{:msg-filter :navigation/prev
                             :handler    handle-prev}]
       api/order-dependency-pred-key
                           deps-pred
       api/post-start-pred-key
                           deps-pred
       api/post-start-action-key
                           register-spa-routes}
      m)))