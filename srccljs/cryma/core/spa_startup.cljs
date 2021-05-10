(ns cryma.core.spa-startup
  (:require [cryma.core.api :as api]))

(defrecord SpaStartup []
  api/IComponent
  (start [module]
    (let [core (:core module)]
      (when-let [event (:start-event module)]
        (api/pub-event core event))
      (when-let [event (api/get-conf module :pub-event)]
        (api/pub-event core event)))
    module)
  (stop [module] module))

(defn new-spa-startup [& [m]]
  (map->SpaStartup
    (merge
      {api/id-key          :spa-startup
       api/static-deps-key [:spa :core]
       api/order-dependency-pred-key
                           (constantly true)}
      m)))