(ns cryma.app.authorizer
  #?(:cljs
     (:require
       [cryma.core.api :as api]
       [cryma.core.msg-filters :as mf]))
  #?(:clj
     (:require
       [cryma.core.api :as api]
       [cryma.core.msg-filters :as mf]
       [datomic.api :as d]))
  #?(:clj
     (:import (java.util Date))))

(def action-opts-key :authorizer/request-settings)
(def nsqn-key :authorizer/nsqn)
#?(:clj
   (def ring-request-user-id-path-key :authorizer/ring-request-user-id-path))

(defn allow [action catortats]
  {:permitted?           true
   :permitted-action     action
   :permitted-categories (if (vector? catortats) catortats [catortats])})

(defn deny [& args]
  (when (seq args)
    (apply println "WARN:" args))
  {:permitted? false})

#?(:clj
   (def direct-category-query
     '[:find
       (pull ?permitted [:db/id :db/ident {:action/business-rule [:db/id :db/ident]}])
       (pull ?cat [:db/id :db/ident])
       :in $ % ?u ?act ?cat-root
       :where
       [?u :sys/entity :entity.sys/user]
       [?act :sys/entity :entity.sys/action]
       [(cryma.app.db/user-access-level $ ?u) ?ual]
       [?ual :access-level/permission ?ualp]
       [?ualp :permission/access-category ?cat]
       (permitted-action ?ualp ?act ?permitted)
       (category ?cat-root ?cat)]))

#?(:clj
   (def action-rules
     '[[(permitted-action ?permission ?act ?permitted)
        [?permission :permission/permitted-action ?act]
        [(identity ?act) ?permitted]]
       [(permitted-action ?permission ?act ?permitted)
        [?a :action/parent ?act]
        (permitted-action ?permission ?a ?permitted)]]))

#?(:clj
   (def direct-category-rules
     (vec
       (concat
         '[[(category ?root ?cat)
            [?cat :sys/entity :entity.sys/access-category]
            [(identity ?cat) ?root]]
           [(category ?root ?cat)
            [?root :sys/access-category ?cat]
            (not [?root :sys/entity :entity.sys/access-category])]
           [(category ?root ?cat)
            [(missing? $ ?root :sys/access-category)]
            [?root :sys/anchor ?anchor]
            (category ?anchor ?cat)
            (not [?root :sys/entity :entity.sys/access-category])]
           [(category ?root ?cat)
            [(missing? $ ?root :sys/access-category)]
            [(missing? $ ?root :sys/anchor)]
            [?root :sys/entity ?e]
            (category ?e ?cat)
            (not [?root :sys/entity :entity.sys/access-category])]]
         action-rules))))

#?(:clj
   (def all-categories-query
     '[:find
       (pull ?permitted [:db/id :db/ident {:action/business-rule [:db/id :db/ident]}])
       (pull ?cat [:db/id :db/ident])
       :in $ % ?u ?act
       :where
       [?u :sys/entity :entity.sys/user]
       [?act :sys/entity :entity.sys/action]
       [(cryma.app.db/user-access-level $ ?u) ?ual]
       [?ual :access-level/permission ?ualp]
       [?ualp :permission/access-category ?cat]
       (permitted-action ?ualp ?act ?permitted)]))

#?(:clj
   (def brules-query
     '[:find
       (pull ?permitted [:db/id :db/ident {:action/business-rule [:db/id :db/ident]}])
       :in $ % ?u ?act ?rule-root
       :where
       [?u :sys/entity :entity.sys/user]
       [?act :sys/entity :entity.sys/action]
       (action-brule ?u ?act ?rule-root ?permitted)]))

#?(:clj
   (def brules-rules
     '[[(action-brule ?u ?act ?rule-root ?permitted)
        [(missing? $ ?act :action/business-rule)]
        [(identity ?act) ?permitted]]
       [(action-brule ?u ?act ?rule-root ?permitted)
        [?act :action/business-rule :brule.app/authorship]
        [?rule-root :sys/author ?u]
        [(identity ?act) ?permitted]]
       [(action-brule ?u ?act ?rule-root ?permitted)
        [?act :action/business-rule :brule.app/authorship]
        [(identity ?rule-root) ?u]
        [(identity ?act) ?permitted]]]))

#?(:clj
   (defn rule-satisfied? [db action user-id rule-opts]
     (let [{action-id :db/ident brule :action/business-rule} action
           brule-id (:db/ident brule)
           {:keys [opts context]} rule-opts]
       (if-let [opts (brule-id opts)]
         (let [rule-root (:rule-root opts)
               rule-root-id (if rule-root (get-in context rule-root :nil) user-id)
               acts (d/q brules-query db brules-rules
                         user-id action-id rule-root-id)]
           (if (seq acts) true false))
         (throw (ex-info "Business rule opts missing for action"
                         {:brule-id  brule-id
                          :action-id action-id}))))))

#?(:clj
   (defn query-permitted-action [db user-id action-id cat-root-id rule-opts]

     (let [cat-root-id (if (keyword? cat-root-id)
                         (d/entid db cat-root-id)
                         cat-root-id)
           [permitted-action permitted-category]
           (first (d/q direct-category-query db direct-category-rules
                       user-id action-id cat-root-id))]
       (if (and permitted-action
                (or (nil? (:action/business-rule permitted-action))
                    (rule-satisfied? db permitted-action user-id rule-opts))
                permitted-category)
         (allow permitted-action permitted-category)
         (deny)))))

#?(:clj
   (defn query-all-permitted-categories [db user-id action-id]
     (let [result (d/q all-categories-query db action-rules user-id action-id)]
       (if (seq result)
         (allow action-id (mapv second result))
         (deny)))))

#?(:cljs
   (defn has-kv [m k v]
     (some (fn [[mk mv]] (cond (= mk k) (= mv v)
                               (map? mv) (has-kv mv k v))) m)))

#?(:cljs
   (defn rule-satisfied? [_ action user-id rule-opts]
     (let [{action-id :db/ident brule :action/business-rule} action
           brule-id (:db/ident brule)
           {:keys [opts context]} rule-opts]
       (if-let [opts (brule-id opts)]
         (let [rule-root (:rule-root opts)
               rule-root-id (if rule-root (get-in context rule-root :nil) user-id)]
           (cond
             (= brule-id :brule.app/authorship)
             (= user-id rule-root-id)))
         (throw (ex-info "Business rule opts missing for action"
                         {:brule-id  brule-id
                          :action-id action-id}))))))

#?(:cljs
   (defn flatten-permissions [principal-info]
     (->>
       principal-info :user/access-level :access-level/permission
       (mapcat
         (fn [perm]
           (map
             (fn [act]
               {:cat  (:permission/access-category perm)
                :act  act
                :pact (loop [a act]
                        (if-let [pa (:action/parent a)]
                          (recur pa) (when (not= a act) a)))
                :action/business-rule
                      (:action/business-rule act)})
             (:permission/permitted-action perm)))))))

#?(:cljs
   (defn query-permitted-action [principal-info user-id action-id cat-root-id rule-opts]
     (let [perms (->>
                   principal-info
                   flatten-permissions
                   (filter (fn [{:keys [cat act pact]}]
                             (and
                               (or (= cat-root-id (:db/id cat))
                                   (= cat-root-id (:db/ident cat)))
                               (or (= action-id (:db/ident act))
                                   (= action-id (:db/ident pact)))))))
           cats (->> perms (map :cat) distinct vec)
           act (->> perms (map :act) distinct first)]
       (if (and act (or (nil? (:action/business-rule act))
                        (rule-satisfied? principal-info
                                         act user-id rule-opts))
                (seq cats))
         (allow act cats)
         (deny)))))

#?(:cljs
   (defn query-all-permitted-categories [principal-info _ action-id]
     (let [cats (->>
                  principal-info
                  flatten-permissions
                  (filter (fn [{:keys [act pact]}]
                            (or (= action-id (:db/ident act))
                                (= action-id (:db/ident pact)))))
                  (map :cat) distinct vec)]
       (if (seq cats)
         (allow action-id cats)
         (deny)))))

(defn get-db [authorizer]
  #?(:clj  (api/get-db (:data-provider authorizer))
     :cljs (api/read-val (:session-reader authorizer) [:principal])))

(defn get-user-id [authorizer user-id]
  (or user-id
      #?(:clj  :sys.users/guest
         :cljs (api/read-val (:session-reader authorizer) [:principal :db/id]))))

(defn authorize-action [authorizer user-id action msg-type]
  (let [user-id (get-user-id authorizer user-id)
        action-id (:qn action)
        db (get-db authorizer)]
    (if-let [{:keys [check-permission category-search-root category-search-root-alt rule-opts]}
             (action-id (action-opts-key authorizer))]
      (let [action-id (or check-permission action-id)]
        (if (= msg-type :event)
          (query-all-permitted-categories db user-id action-id)
          (if category-search-root
            (if-let [cat-root-id
                     (if (or (number? category-search-root) (keyword? category-search-root))
                       category-search-root
                       (get-in action category-search-root
                               (when category-search-root-alt
                                 (get-in action category-search-root-alt))))]
              (query-permitted-action db user-id action-id cat-root-id
                                      {:opts rule-opts :context action})
              (deny "Authorizer: NIL category-root-id for action: " action
                    " path:" category-search-root))
            (query-all-permitted-categories db user-id action-id))))
      (deny "Authorizer: Unknown action permission denied: " action))))

(defn serve-authorize [msg-type authorizer request]
  (let [{:keys [msg user-id remote?]} request]
    (api/respond-success
      (:core authorizer)
      request (authorize-action authorizer user-id msg msg-type))))


;; DUMB SUBSRIPTION AUTH
;;======================
(defn filter-rule-fn [attr using rule]
  (if (and (vector? rule) (= (second rule) using))
    {attr (first rule)}
    rule))

(defn filter->permission-checks [user-id filt]
  (let [{:keys [qn entity-id uid category-search-root]}
        (->>
          filt
          (map (partial filter-rule-fn :qn [:qn]))
          (map (partial filter-rule-fn :entity-id [[:entity :sys/entity :db/ident]]))
          (map (partial filter-rule-fn :category-search-root [[:anchor-path 0]]))
          (map (partial filter-rule-fn :uid [:addressees]))
          (filter map?)
          (into {}))]
    {:cat-root-id (if (#{:notifications.events/profile-event} qn)
                    :access-category/open category-search-root)
     :action-id   (cond
                    (and (#{:entity.app/comment} entity-id)
                         (#{:notifications.events/entity-added
                            :notifications.events/entity-changed} qn))
                    :action.app/read-post-discussion
                    (and (#{:entity.app/post} entity-id)
                         (#{:notifications.events/entity-changed} qn))
                    :action.app/read-post
                    (and (#{:entity.app/hub} entity-id)
                         (#{:notifications.events/entity-changed} qn))
                    :action.app/read-hub
                    (and
                      (= uid #{user-id})
                      (#{:notifications.events/profile-event} qn))
                    :action.app/read-user-profile
                    :else
                    ::denied)}))

(defn check-sub-filter-perm [authorizer user-id perm]
  (let [{:keys [data-provider logger]} authorizer
        user-id (or user-id :sys.users/guest)
        db (api/get-db data-provider)
        action-id (:action-id perm)
        cat-root-id (:cat-root-id perm)]
    (logger :debug "check-sub-filter-perm\n\tuser-id\t(:action-id perm)\t(:cat-root-id perm)\n"
            user-id "\t" action-id "\t" cat-root-id "\n")
    (query-permitted-action db user-id action-id cat-root-id nil)))

(defn do-authorize-subscription
  [authorizer request]
  (let [{:keys [msg user-id]} request
        to-check (map (partial filter->permission-checks user-id) (:filters msg))]
    (if (some (fn [chk] (= ::denied (:action-id chk))) to-check)
      (deny)
      (if (every? (fn [perm] (:permitted? (check-sub-filter-perm authorizer user-id perm))) to-check)
        (allow (:qn request) [])
        (deny)))))

(defn serve-authorize-subscription
  [app request]
  (api/respond-success
    (:core app)
    request
    (do-authorize-subscription app request)))

;;======================

#?(:cljs
   (defn serve-check-permission [authorizer request]
     (let [{:keys [core session-reader]} authorizer
           {:keys [action-id access-category-id]} request
           principal-info (api/read-val session-reader [:principal])]
       (api/respond-success
         core request
         (query-permitted-action
           principal-info (:db/id principal-info)
           action-id access-category-id
           {:opts    {:brule.app/authorship {:rule-root [:sys/author]}}
            :context request})))))

(defn serve-get-permitted-categories
  [authorizer request]
  (let [{:keys [localizer core]} authorizer
        {:keys [action-id user-id search-string]
         :or   {user-id :sys.users/guest}} request
        db (get-db authorizer)
        action-id (or (:check-permission (action-id (action-opts-key authorizer))) action-id)
        cats (:permitted-categories
               (query-all-permitted-categories db user-id action-id))
        cats (mapv (fn [cat]
                     (assoc cat
                       :access-category/title
                       (when localizer (localizer (:db/ident cat)))))
                   cats)]
    (api/respond-success
      core request
      (if (and localizer (seq search-string))
        (filterv
          (fn [cat]
            (> (.indexOf
                 (.toLowerCase (:access-category/title cat))
                 (.toLowerCase search-string))
               -1))
          cats)
        cats))))

(defn register-action-authorization-settings
  [authorizer units]
  (let [nsqn (nsqn-key authorizer)]
    (->>
      (mapcat api/request-servers-key units)
      (map (fn [serv-spec]
             {:qn                 (mf/msg-filter->qn (:msg-filter serv-spec))
              :authorization-opts (:authorization-opts serv-spec)}))
      (filter (fn [spec] (when-let [qn (:qn spec)]
                           (if (set? nsqn)
                             (some? (nsqn (mf/get-msqn qn)))
                             (= nsqn (mf/get-msqn qn))))))
      (map (fn [{:keys [qn authorization-opts]}]
             (if authorization-opts
               [qn authorization-opts]
               (throw (ex-info "Missing :authorization-opts for request server declaration. Authorization opts are required for server by authorizer module. Opts are map with optional keys: category-search-root - path in request map to start search category."
                               {:authorizer-namespace-qn nsqn
                                :msg-filter-qn           qn})))))
      (into {})
      (assoc authorizer action-opts-key))))

(defrecord Authorizer []
  #?(:clj api/IClientStateProvider)
  #?(:clj (provide-client-state [authorizer _]
            {action-opts-key (action-opts-key authorizer)}))
  api/IModule
  api/IComponent
  (start [authorizer]
    (update authorizer action-opts-key merge (api/get-conf authorizer action-opts-key)))
  (stop [authorizer] (dissoc authorizer action-opts-key)))

(defn deps-pred [unit]
  (when-let [request-servers (api/request-servers-key unit)]
    (some :authorization-opts request-servers)))

(defn new-authorizer [& [m]]
  (map->Authorizer
    (merge
      #?(:clj
         {api/id-key          :authorizer
          api/static-deps-key [:data-provider]
          api/request-servers-key
                              [{:msg-filter  :authorizer/authorize-request
                                :parallelism {:n 8}
                                :handler     (partial serve-authorize :request)}
                               {:msg-filter  :authorizer/authorize-event
                                :parallelism {:n 2}
                                :handler     (partial serve-authorize :event)}
                               {:msg-filter :authorizer/get-permitted-categories
                                :handler    serve-get-permitted-categories}
                               {:msg-filter :authorizer/authorize-subscription
                                :handler    serve-authorize-subscription}]
          api/order-reverse-dependency-pred-key
                              deps-pred
          api/pre-start-pred-key
                              deps-pred
          api/pre-start-action-key
                              register-action-authorization-settings
          ring-request-user-id-path-key
                              [:session :user-id]

          nsqn-key            :action.app
          action-opts-key     {}}

         :cljs
         {api/id-key          :authorizer
          api/static-deps-key [:session-reader :localizer]
          api/request-servers-key
                              [{:msg-filter  :authorizer/authorize-request
                                :parallelism {:n 8}
                                :handler     (partial serve-authorize :request)}
                               {:msg-filter :authorizer/check-permission
                                :handler    serve-check-permission}
                               {:msg-filter  :authorizer/authorize-event
                                :parallelism {:n 2}
                                :handler     (partial serve-authorize :event)}
                               {:msg-filter :authorizer/get-permitted-categories
                                :handler    serve-get-permitted-categories}]
          nsqn-key            :action.app
          action-opts-key     {}})
      m)))
