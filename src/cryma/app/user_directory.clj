(ns cryma.app.user-directory
  (:require [cryma.core.api :as api]
            [datomic.api :as d]
            [cryma.app.db :as db]
            [cryma.core.schema :as sch]
            [cryma.core.tools :as tools]))

(def principal-pull-pattern-key :user-directory/principal-pull-pattern)

(defn ual-pattern [pull-pattern]
  (some
    (fn [entry]
      (cond
        (= entry :user/access-level)
        [:db/id]
        (and (map? entry)
             (contains? entry :user/access-level))
        (:user/access-level entry))) pull-pattern))

(defn prepare-principal [db principal pull-pattern]
  (if-let [ual-pattern (ual-pattern pull-pattern)]
    (let [ual-id (db/user-access-level db (:db/id principal))]
      (if (= ual-id (get-in principal [:user/access-level :db/id]))
        principal
        (assoc principal :user/access-level (d/pull db ual-pattern ual-id))))
    principal))

(defn validate-pwd-hash [userdir pwd hash]
  (let [password-hasher (:password-hasher userdir)]
    (if (nil? hash)
      (do
        (api/verify-hash
          password-hasher
          "" "$2a$10$iTs7JJXThAsw1af99IKLwunG.MpJTAQSMgjtrtNlqNuPqyAsJoB3C")
        false)
      (api/verify-hash password-hasher pwd hash))))

(defn authenticate-user [userdir credentials]
  (let [{:keys [login pwd]} credentials
        db-provider (:data-provider userdir)
        db (api/get-db db-provider)
        pull-pattern (principal-pull-pattern-key userdir)
        [hash principal] (-> '[:find ?hash (pull ?e pull-pattern)
                               :in $ ?login pull-pattern
                               :where
                               [?e :user/login ?login]
                               [?e :user/pwd-hash ?hash]
                               (not [?e :sys/archived true])
                               (not [?e :user/is-sys true])]
                             (d/q db login pull-pattern)
                             first)
        hash-valid? (validate-pwd-hash userdir pwd hash)]
    (when hash-valid? (prepare-principal db principal pull-pattern))))

(defn serve-authenticate-user [userdir request]
  (let [core (:core userdir)]
    (if-let [principal (authenticate-user userdir (:credentials request))]
      (api/respond-success core request principal)
      (api/respond-error core request {:code :authentication/wrong-credentials}))))

(defn serve-get-user-info [userdir request]
  (let [core (:core userdir)
        user-id (:user-id request)
        db-provider (:data-provider userdir)
        db (api/get-db-filtered db-provider)
        pull-pattern (principal-pull-pattern-key userdir)]
    (api/respond-success
      core request
      (prepare-principal
        db (assoc (d/pull db pull-pattern user-id) :db/id user-id)
        pull-pattern))))

(defn get-users [db request & [with-info?]]
  (let [data
        (db/fetch-data
          db
          (-> request
              (update
                :pattern
                #(or %
                     [:*
                      {:sys/entity [:db/id :db/ident]}
                      {:sys/access-category [:db/id :db/ident]}
                      {:user/access-level [:db/id :db/ident :access-level/name]}
                      {:sys/author [:db/id  :user/nickname]}
                      {:sys/_anchor [:*
                                     {:sys/entity [:db/id :db/ident]}
                                     {:sys/author [:db/id :user/nickname]}]}]))
              (update
                :search-fields
                #(or %
                     [:db/id :user/nickname :user/about]))
              (assoc :entity :entity.sys/user))
          with-info?)]
    data))

(defn get-user [db request]
  (first (get-users db request)))

(defn filtered-db [db-provider]
  (let [db (api/get-db db-provider)]
    (d/filter db (db/exclude-attr-filter-ctor :user/pwd-hash db))))

(defn serve-browse-users [userdir request]
  (let [core (:core userdir)
        db-provider (:data-provider userdir)
        db (filtered-db db-provider)]
    (api/respond-success
      core request
      (get-users db request {:value-field :value}))))

(defn serve-read-user [userdir request]
  (let [core (:core userdir)
        db-provider (:data-provider userdir)
        db (filtered-db db-provider)]
    (api/respond-success
      core request
      (get-user db request))))

(defn prepare-pwd-field [userdir user]
  (let [{:keys [password-hasher]} userdir]
    (dissoc
      (if-let [pwd (:user/pwd user)]
        (assoc user :user/pwd-hash (api/hash-value password-hasher pwd))
        user)
      :user/pwd)))

(defn request->user [userdir request author-id operation]
  (let [db (api/get-db-filtered (:data-provider userdir))
        pwd-preparer (partial prepare-pwd-field userdir)
        user-raw (->
                   request
                   (select-keys [:db/id
                                 :user/login
                                 :user/pwd
                                 :user/nickname
                                 :user/access-level
                                 :user/access-expiry
                                 :user/is-sys
                                 :user/about
                                 :sys/access-category])
                   pwd-preparer)]
    (if (= :create operation)
      (db/prepare-entity-data db user-raw :entity.sys/user author-id)
      (tools/prepare-value user-raw))))

(defn upsert-user [userdir request]
  (let [operation (if (= :user-directory/create-user (:qn request))
                    :create :update)
        author-id (:sys/author request)
        user (request->user userdir request author-id operation)
        tx-fn (case operation
                :create :app/add-entity
                :update :app/update-entity)
        conn (api/get-conn (:data-provider userdir))
        tx-result @(d/transact conn [[tx-fn user author-id]])
        dbid (db/get-tx-id user tx-result)
        db-after (:db-after tx-result)]
    (get-user (d/filter db-after
                        (db/exclude-attrs-filter-ctor #{:user/login :user/pwd-hash} db-after))
              {:filter {:db/id dbid}})))

(defn field-value-unique? [field userdir request]
  (let [db (api/get-db-filtered (:data-provider userdir))
        v (get request field)]
    (nil? (:db/id (d/entity db [field v])))))

(defn unicity-error? [userdir request]
  (let [{:keys [:db/id :user/login :user/nickname]} request
        db (api/get-db (:data-provider userdir))
        user (when id (d/pull db [:user/nickname :user/login] id))]
    (cond
      (and login (not= login (:user/login user)) (not (field-value-unique? :user/login userdir request)))
      {:code  :constraint.violation.unique/non-unique-login
       :field :user/login}
      (and nickname (not= nickname (:user/nickname user)) (not (field-value-unique? :user/nickname userdir request)))
      {:code  :constraint.violation.unique/non-unique-nickname
       :field :user/nickname})))

(defn serve-create-user [userdir request]
  (let [{:keys [core]} userdir]
    (if-let [error (unicity-error? userdir request)]
      (api/respond-error core request error)
      (api/respond-success core request (upsert-user userdir request)))))

(defn wrong-dbid-error? [userdir request]
  (when (or (nil? (:user/login
                    (d/entity (api/get-db (:data-provider userdir))
                              (:db/id request))))
            (= :sys.users/guest (:db/id request)))
    {:code  :wrong-dbid
     :field :db/id}))

(defn serve-update-user [userdir request]
  (let [{:keys [core]} userdir]
    (if-let [error (or (unicity-error? userdir request) (wrong-dbid-error? userdir request))]
      (api/respond-error core request error)
      (api/respond-success core request (upsert-user userdir request)))))

(defn serve-remove-user [userdir request]
  (let [{:keys [core data-provider]} userdir]
    (if-let [error (wrong-dbid-error? userdir request)]
      (api/respond-error core request error)
      (do
        @(d/transact
           (api/get-conn data-provider)
           (db/annotate-tx [:db.fn/retractEntity (:db/id request)]
                           (:sys/author request)))
        (api/respond-success core request)))))

(defn serve-update-user-credentials [userdir request]
  (let [{:keys [core]} userdir
        {:keys [:db/id :user/login-current :user/pwd-current]} request
        user (d/pull (api/get-db (:data-provider userdir))
                     [:db/id :user/pwd-hash :user/is-sys :sys/archived]
                     [:user/login login-current])
        valid? (and
                 (or (nil? id) (= id (:db/id user)))
                 (not (:user/is-sys user))
                 (not (:user/archived user))
                 (some? (:user/pwd-hash user))
                 (validate-pwd-hash userdir pwd-current (:user/pwd-hash user)))]
    (if-not valid?
      (api/respond-error
        core request
        (if (and user (or (nil? id) (= id (:db/id user))))
          {:code  :authentication/wrong-pwd
           :field :user/pwd-current}
          {:code  :authentication/wrong-login
           :field :user/login-current}))
      (if-let [error (unicity-error? userdir request)]
        (api/respond-error core request error)
        (api/respond-success core request
                             (upsert-user userdir
                                          (-> request
                                              (select-keys [:qn :sys/author :user/pwd :user/login])
                                              (assoc :db/id (:db/id user)))))))))

(def long-or-kw-schema
  [[:data/alternative
    [[:data/type :data-type/long]]
    [[:data/type :data-type/keyword]]]])

(def nickname-schema
  [[:data/type :data-type/string]
   [:string/length {:min 4 :max 100}]])

(def login-schema
  [[:data/type :data-type/string]
   [:string/length {:min 4 :max 100}]])

(def pwd-schema
  [[:data/type :data-type/string]
   [:string/length {:min 8 :max 100}]
   #_[:string/regexp-required "\\d"]
   #_[:string/regexp-required "\\w"]
   #_[:string/regexp-required "[^\\d\\w]"]])

(def create-user-schema
  [[:data/type :data-type/map]
   [:map/spec-closed
    {:sys/author          (sch/required long-or-kw-schema)
     :user/login          (sch/required login-schema)
     :user/pwd            (sch/required pwd-schema)
     :user/nickname       (sch/required nickname-schema)
     :user/about          [[:data/type :data-type/string]]
     :user/access-level   (sch/required long-or-kw-schema)
     :user/access-expiry  [[:data/type :data-type/instant]]
     :sys/access-category [[:data/type :data-type/keyword]]
     :user/is-sys         [[:data/type :data-type/boolean]]}]])

(def update-user-schema
  [[:data/type :data-type/map]
   [:map/spec-closed
    {:sys/author          (sch/required long-or-kw-schema)
     :db/id               (sch/required long-or-kw-schema)
     :user/login          login-schema
     :user/pwd            pwd-schema
     :user/nickname       [[:data/type :data-type/string]]
     :user/about          [[:data/type :data-type/string]]
     :user/access-level   long-or-kw-schema
     :user/access-expiry  [[:data/type :data-type/instant]]
     :sys/access-category [[:data/type :data-type/keyword]]
     :user/is-sys         [[:data/type :data-type/boolean]]}]])

(def remove-user-schema
  [[:data/type :data-type/map]
   [:map/spec-closed
    {:sys/author (sch/required long-or-kw-schema)
     :db/id      (sch/required [[:data/type :data-type/long]])}]])

(def update-user-credentials-schema
  [[:data/type :data-type/map]
   [:map/spec-closed
    {:sys/author         (sch/required long-or-kw-schema)
     :db/id              long-or-kw-schema
     :user/login-current (sch/required login-schema)
     :user/pwd-current   (sch/required pwd-schema)
     :user/login         (sch/required login-schema {:data-requirement/alternative :user/pwd})
     :user/pwd           (sch/required pwd-schema {:data-requirement/alternative :user/login})
     }]])

(defrecord UserDirectory []
  api/IModule
  api/IComponent
  (start [userdir]
    (update userdir principal-pull-pattern-key
            (fn [pattern] (-> pattern (conj :db/id) distinct vec))))
  (stop [userdir] userdir))

(def search-schema
  [[:data/type :data-type/map]
   [:map/spec {:search-string
                        [:data/type :data-type/string]
               :pattern [:data/type :data-type/vector]
               :limit   [:data/type :data-type/number]
               :offset  [:data/type :data-type/number]
               :filter  [:data/type :data-type/map]}]])

(def read-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:filter
     [:map/spec-closed
      {:db/id [[:data/requirement :data-requirement/required]
               [:data/type :data-type/long]]}]}]])

(defn new-user-directory [& [m]]
  (map->UserDirectory
    (merge
      {api/id-key
       :users-directory
       api/static-deps-key
       [:data-provider :password-hasher]
       api/request-servers-key
       [{:msg-filter      :user-directory/authenticate-user
         :handler         #'serve-authenticate-user
         :validation-opts {:schema [[:map/spec
                                     {:credentials
                                      [:map/spec
                                       {:login [[:data/type :data-type/string]
                                                [:string/not-empty]]
                                        :pwd   [[:data/type :data-type/string]
                                                [:string/not-empty]]}]}]]}}
        {:msg-filter      :user-directory/get-user-info
         :handler         #'serve-get-user-info
         :validation-opts {:schema [[:map/spec
                                     {:user-id [[:data/requirement :data-requirement/required]]}]]}}
        {:msg-filter      :user-directory/browse-users
         :handler         #'serve-browse-users
         :validation-opts {:schema search-schema}}
        {:msg-filter      :user-directory/read-user
         :handler         #'serve-read-user
         :validation-opts {:schema read-schema}}
        {:msg-filter      :user-directory/create-user
         :handler         #'serve-create-user
         :validation-opts {:schema create-user-schema}}
        {:msg-filter      :user-directory/update-user
         :handler         #'serve-update-user
         :validation-opts {:schema update-user-schema}}
        {:msg-filter      :user-directory/remove-user
         :handler         #'serve-remove-user
         :validation-opts {:schema remove-user-schema}}
        {:msg-filter      :user-directory/update-user-credentials
         :handler         #'serve-update-user-credentials
         :validation-opts {:schema update-user-credentials-schema}}]
       principal-pull-pattern-key
       [:db/id {:sys/entity [:db/id :db/ident]}
        :user/nickname :user/login :user/is-sys :sys/archived
        :user/access-expiry
        {:user/access-level
         [:db/ident
          {:access-level/permission
           [{:permission/access-category [:db/id :db/ident]}
            {:permission/permitted-action [:db/ident
                                           {:action/parent 5}
                                           {:action/business-rule [:db/ident]}]}]}]}]}
      m)))
