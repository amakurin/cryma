(ns cryma.core.spa-uri-sync
  #?(:cljs
     (:require
       [goog.history.Html5History.TokenTransformer]
       [goog.events :as events]
       [goog.history.EventType]
       [goog.history.Html5History]
       [cljs.tools.reader :as reader]
       [cryma.core.api :as api]
       [cryma.core.msg-filters :as mf]
       [cryma.core.dtl.core :as dtl]
       [cryma.core.dtl.transformers.str :as str]
       [cryma.core.dtl.transformers.mapping :as mapping]
       [bidi.bidi :as bidi]
       [cemerick.url :as url])
     :clj
     (:require
       [cryma.core.api :as api]
       [bidi.bidi :as bidi]
       [cemerick.url :as url]
       [clojure.tools.reader.edn :as reader]
       [cryma.core.tools :as tools]))
  #?(:cljs
     (:import
       [goog.history Html5History]
       [goog.history EventType])))

(defn replace-qualifiers [src & [reverse?]]
  (let [direct-mapping {keyword   :function-quaifier/keyword
                        long      :function-quaifier/long
                        bidi/uuid :function-quaifier/uuid}
        mapping (if reverse?
                  (->> direct-mapping (map (fn [[k v]] [v k])) (into {}))
                  direct-mapping)
        compare-set (set (keys mapping))]
    (reduce
      (fn [res item] (cond
                       (vector? item)
                       (conj res (replace-qualifiers item reverse?))
                       (compare-set item)
                       (conj res (get mapping item))
                       :else
                       (conj res item)))
      [] src)))

(defn resolve-routes [routes]
  (mapv
    (fn [path]
      (first
        (reduce
          (fn [res x]
            (cond
              (empty? res) [::first (if (:action-qn x) (fn [] x) x)]
              (= ::first (first res)) [[x (last res)]]
              :else [[x res]]))
          []
          (reverse path))))
    routes))

;;====================== CLJ
#?(:clj
   (defrecord SpaUriSynchronizer []
     api/IClientStateProvider
     (provide-client-state [uri-sync _]
       {:routes (:routes uri-sync)})
     api/IModule
     api/IComponent
     (start [uri-sync] uri-sync)
     (stop [uri-sync] uri-sync)))

#?(:clj
   (defn register-routes
     [uri-sync units]
     (->>
       units
       (mapcat api/routes-key)
       (filter (fn [route] (and (some #{:get} route)
                                (map? (peek route))
                                (seq (select-keys (peek route) [:action-qn :uri-rule])))))
       (map (fn [route] (vec (remove #{:get} route))))
       (map (fn [route] (conj (pop route) (dissoc (peek route) :handler))))
       (mapv replace-qualifiers)
       (assoc uri-sync :routes))))

#?(:clj
   (defn new-spa-uri-sync [& [m]]
     (map->SpaUriSynchronizer
       (merge
         {api/id-key               :spa-uri-sync
          api/pre-start-pred-key   (fn [u] (some? (api/routes-key u)))
          api/pre-start-action-key register-routes}
         m))))

;;====================== CLJS

#?(:cljs
   (defn browser? [] (try (not (nil? js/window)) (catch :default e false))))

#?(:cljs
   (defn history-supported? [] (and (browser?) (Html5History/isSupported))))

#?(:cljs
   (defn set-history-token [history link]
     (when history
       (when-not (= link (.getToken history))
         (.setToken history link)))))

#?(:cljs
   (defn history-event [event]
     {:token       (.-token event)
      :navigation? (.-isNavigation event)}))

(defn parse-query-param [rules [k v]]
  (let [k (keyword k)
        ktype (when rules (get rules k))]
    [k (if (and ktype (not= ktype :string)) (reader/read-string v) v)]))

#?(:cljs
   (defn ugliest-query-parsing-ever-seen [uri-sync url]
     (let [rules (:query-params-type-rules uri-sync)]
       (->>
         url
         (re-find #"(?:\?)[^\?]*$")
         rest
         (apply str)
         url/query->map
         (map (partial parse-query-param rules))
         (into {})))))

#?(:cljs
   (defn local-handler? [uri-sync path]
     (when-let [match-context (bidi/match-route (:routes uri-sync) path)]
       (let [qparams (ugliest-query-parsing-ever-seen uri-sync path)
             {:keys [event-spec] :as match-context}
             (-> match-context
                 (assoc :params (merge qparams (:route-params match-context)))
                 (dissoc :route-params)
                 (assoc :event-spec ((:handler match-context))))
             {:keys [action-qn action-rule]} event-spec]
         (merge
           {:qn action-qn}
           (when action-rule
             (dtl/transform match-context action-rule mapping/transformer)))))))

#?(:cljs
   (defn- handle-history-event [uri-sync event]
     (let [{:keys [core sync-uri? initial-uri-handled? logger]} uri-sync
           event (history-event event)]
       (when (and sync-uri? (or (:navigation? event) (not @initial-uri-handled?)))
         (when-not @initial-uri-handled? (reset! initial-uri-handled? true))
         (logger :debug "history-event: " (:token event)
                 "(local-handler? uri-sync (:token event)):" (local-handler? uri-sync (:token event)))
         (if-let [core-action (local-handler? uri-sync (:token event))]
           (api/pub-event core core-action)
           (.replace (.-location js/window) (:token event)))))))

#?(:cljs
   (defn build-transformer
     "Custom transformer is needed to replace query parameters, rather
     than adding to them.
     See: https://gist.github.com/pleasetrythisathome/d1d9b1d74705b6771c20"
     []
     (let [transformer (goog.history.Html5History.TokenTransformer.)]
       (set! (.. transformer -retrieveToken)
             (fn [path-prefix location]
               (str (.-pathname location) (.-search location))))
       (set! (.. transformer -createUrl)
             (fn [token path-prefix location]
               (str path-prefix token)))
       transformer)))

#?(:cljs
   (defn init-history [uri-sync]
     (if-let [history (and (history-supported?)
                           (Html5History. js/window (build-transformer)))]
       (let [uri-sync (assoc uri-sync :history history)]
         (events/listen history EventType/NAVIGATE (partial handle-history-event uri-sync))
         (doto history
           (.setUseFragment false)
           (.setPathPrefix "")
           (.setEnabled true))
         uri-sync)
       uri-sync)))

#?(:cljs
   (defn handle-navigated [uri-sync event]
     (when (:sync-uri? uri-sync)
       (let [{:keys [logger history]} uri-sync
             nav-event (:event event)
             nav-uri (uri-sync nav-event)
             cur-uri (.getToken history)]
         (logger :debug "handle-navigated: " (:qn nav-event)
                 "nav.uri:" nav-uri
                 "cur.uri:" cur-uri)
         (when nav-uri
           (set-history-token history nav-uri))))))

#?(:cljs
   (defn register-spa-routes [uri-sync units]
     (->>
       (mapcat api/routes-key units)
       (map (fn [spec]
              {:qn       (mf/msg-filter->qn (:msg-filter spec))
               :uri-rule (:uri-rule spec)}))
       (filter :uri-rule)
       (map (fn [{:keys [qn uri-rule]}] [qn uri-rule]))
       (into {})
       (assoc uri-sync :uri-rules))))

#?(:cljs
   (defrecord SpaUriSynchronizer []
     IFn
     (-invoke [uri-sync event]
       (when-let [uri-rule (get-in uri-sync [:uri-rules (:qn event)])]
         (if (string? uri-rule)
           uri-rule
           (dtl/transform event uri-rule str/transformer
                          (fn [_ v] (if (keyword? v) (name v) v))))))
     api/IModule
     api/IComponent
     (start [uri-sync]
       (let [uri-sync
             (->>
               (api/get-conf uri-sync :routes)
               (map (fn [route] (replace-qualifiers route :reverse!)))
               (update uri-sync :routes (comp vec distinct concat)))]
         (-> uri-sync
             (update :uri-rules merge
                     (->> (:routes uri-sync)
                          (map last)
                          (map (fn [{:keys [action-qn uri-rule]}]
                                 (when action-qn [action-qn uri-rule])))
                          (remove nil?)
                          (into {})))
             (update :routes
                     (fn [routes]
                       ["" (->> routes
                                (filterv (fn [route] (:action-qn (peek route))))
                                resolve-routes)]))
             (assoc :initial-uri-handled? (atom false))
             init-history)))
     (stop [uri-sync] uri-sync)))

#?(:cljs
   (defn deps-pred [unit]
     (when-let [routes (api/routes-key unit)]
       (some :uri-rules routes))))

#?(:cljs
   (defn new-spa-uri-sync [& [m]]
     (map->SpaUriSynchronizer
       (merge
         {api/id-key          :spa-uri-sync
          api/static-deps-key [:core]
          api/event-subs-key  [{:msg-filter :spa/navigated
                                :handler    handle-navigated}]
          api/order-reverse-dependency-pred-key
                              deps-pred
          api/pre-start-pred-key
                              deps-pred
          api/pre-start-action-key
                              register-spa-routes
          :sync-uri?          true}
         m))))