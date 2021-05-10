(ns cryma.core.request-validator-pipe
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]]))
  (:require
    [cryma.core.api :as api]
    [cryma.core.schema :as sch]
    [cryma.core.msg-filters :as mf]
    #?(:clj
    [clojure.core.async :as async :refer [go]]
       :cljs [cljs.core.async :as async])))

(def action-opts-key :validator/request-settings)

(defn release [request ch]
  (async/put! ch request)
  (async/close! ch))

(defn respond-error
  [core ch request errors]
  (api/respond-error core request (sch/flatten-map-errors errors))
  (async/close! ch))

(defn validate-request [spec request ch]
  (let [rvp (:module spec)
        core (:core rvp)
        action-id (:qn request)]
    (if-let [schema (get-in (action-opts-key rvp) [action-id :schema])]
      (if-let [errors (sch/errors? schema (dissoc request :qn :target))]
        (respond-error core ch request errors)
        (release request ch))
      (release request ch))))

(defn register-action-validation-settings
  [rvp units]
  (->>
    (mapcat api/request-servers-key units)
    (map (fn [serv-spec]
           {:qn              (mf/msg-filter->qn (:msg-filter serv-spec))
            :validation-opts (:validation-opts serv-spec)}))
    (filter :validation-opts)
    (map (fn [{:keys [qn validation-opts]}]
           (if (:schema validation-opts)
             [qn validation-opts]
             (throw (ex-info "Missing :schema in :validation-opts for request server declaration. Validation opts requires :schema key, and support :scope key as set of #{:server :client}."
                             {:msg-filter-qn   qn
                              :validation-opts validation-opts})))))
    (into {})
    (assoc rvp action-opts-key)))

(defrecord ValidationPipe []
  #?(:clj api/IClientStateProvider)
  #?(:clj (provide-client-state [rvp _]
            {action-opts-key (->>
                               (action-opts-key rvp)
                               (filter (fn [[_ v]] ((set (:scope v)) :client)))
                               (map (fn [[k v]] [k (dissoc v :scope)]))
                               (into {}))}))
  api/IModule
  api/IComponent
  (start [rvp] rvp
    (update rvp action-opts-key
            merge (api/get-conf rvp action-opts-key)))
  (stop [rvp] (dissoc rvp action-opts-key)))

(defn deps-pred [unit]
  (when-let [request-servers (api/request-servers-key unit)]
    (some :validation-opts request-servers)))

(defn new-validation-pipe [& [m]]
  (map->ValidationPipe
    (merge
      {api/id-key :validation-pipe
       api/request-pipelines-key
                  [{:msg-filter mf/empty-filter
                    :priority   (or (:pipe-priority m) 100)
                    :handler    validate-request}]
       api/order-reverse-dependency-pred-key
                  deps-pred
       api/pre-start-pred-key
                  deps-pred
       api/pre-start-action-key
                  register-action-validation-settings}
      m)))
