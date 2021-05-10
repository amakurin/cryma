(ns cryma.app.app
  (:require
    [datomic.api :as d]
    [cryma.app.db :as db]
    [cryma.app.user-directory :as users]
    [cryma.app.posts :as posts]
    [cryma.app.hubs :as hubs]
    [cryma.app.comments :as comments]
    [cryma.app.anchored :as anch]
    [cryma.core.api :as api]
    [cryma.app.db :as db]
    [clojure.core.async :as async]
    [cryma.core.schema :as sch]
    [cryma.core.tools :as tools])
  (:import (datomic Datom)))

(defn auth-result [request & ks]
  (let [auth-result (:auth-result (meta request))]
    (if (seq ks)
      (get-in auth-result ks)
      auth-result)))

(defn filtered-db [data-provider filt-ctors]
  (api/get-db-filtered
    data-provider
    (filterv some? filt-ctors)))

(defn browsable-db [data-provider request & filt-ctors]
  (filtered-db data-provider
               (concat
                 [(db/new-sys-anchor-filter-ctor
                    #{:entity.app/rating
                      :entity.app/favorite
                      :entity.app/sub}
                    (auth-result request :user-id))]
                 filt-ctors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn handle-hub-search
  [module request]
  (let [{:keys [core data-provider]} module
        params
        (merge
          (select-keys
            request [:search-string :filter :pattern :order :limit :offset])
          {:entity        :entity.app/hub
           :search-fields (:search-fields request
                            [:hub/name :hub/description])})]
    (api/respond-success
      core request (db/fetch-data (api/get-db data-provider) params))))

(defn handle-tag-search
  [module request]
  (let [{:keys [core data-provider]} module
        params
        (merge
          (select-keys
            request [:search-string :filter :pattern :order :limit :offset])
          {:entity        :entity.app/tag
           :search-fields (:search-fields request [:tag/title])})]
    (api/respond-success
      core request (db/fetch-data (api/get-db data-provider) params))))

;==================== ANCHOREDs ====================
(defn handle-anchor
  [anchor-entity-ident anchored-entity-ident module request]
  (let [{:keys [core data-provider]} module]
    (let [value (:value request)
          retraction? (:retraction? request)
          user-id (auth-result request :user-id)
          conn (api/get-conn data-provider)
          db (d/db conn)
          anchor-id (if retraction?
                      (get-in
                        (d/pull db [{:sys/anchor [:db/id]}] (:db/id value))
                        [:sys/anchor :db/id])
                      (:sys/anchor value))
          result
          (if retraction?
            (anch/remove-anchor conn user-id value)
            (anch/create-anchor conn user-id anchored-entity-ident value))]
      (api/respond-success core request result)
      (api/pub-event
        core
        {:qn              (if retraction?
                            :event.app/anchored-retracted
                            :event.app/anchored-added)
         :anchor-entity   anchor-entity-ident
         :anchored-entity anchored-entity-ident
         :anchor-id       anchor-id
         :anchored        result}))))

;==================== COMMENTs ====================
(defn handle-write-comment
  [module request]
  (let [{:keys [core data-provider]} module
        result (comments/write-comment
                 (api/get-conn data-provider)
                 (auth-result request :user-id)
                 (:value request))]
    (api/respond-success core request result)
    (api/pub-event
      core
      {:qn      :event.app/comment-added
       :comment result})))

(defn handle-edit-comment
  [module request]
  (let [{:keys [core data-provider]} module
        result (comments/edit-comment
                 (api/get-conn data-provider)
                 (auth-result request :user-id)
                 (:value request))]
    (api/respond-success core request result)
    (api/pub-event
      core
      {:qn      :event.app/comment-changed
       :comment result})))

(defn comments-categories-filter [category-ids db]
  (let [comments-set #{(d/entid db :entity.app/comment) :entity.app/comment}
        fav-set (->>
                  (d/q
                    '[:find
                      ?e
                      :in $ [?cats ...]
                      :where
                      [?e :sys/entity :entity.app/comment]
                      [(datomic.api/invoke $ :app/root-anchor-fn $ ?e) ?root]
                      [?root :sys/access-category ?cats]]
                    db category-ids)
                  (mapcat identity) set)]
    (fn [plaindb ^Datom datom]
      (let [id (.e datom)
            entity (d/entity plaindb id)]
        (if (comments-set (:sys/entity entity))
          (some? (fav-set id))
          true)))))

(defn handle-browse-comments
  [module request]
  (let [user-id (or (auth-result request :user-id) :sys.users/guest)
        request (update-in request [:filter :sys/author] (fn [author-id] (or author-id user-id)))
        {:keys [core data-provider]} module
        acces-cats (mapv :db/id (auth-result request :permitted-categories))
        db (browsable-db data-provider request
                         (partial comments-categories-filter acces-cats))]
    (api/respond-success
      core request
      (comments/get-comments-plain db request {:value-field :value}))))

(defn handle-read-post-discussion
  [module request]
  (let [{:keys [core data-provider]} module
        db (browsable-db data-provider request)]
    (api/respond-success
      core request
      (comments/get-comments db request))))

(defn handle-rate-comment [module request]
  (handle-anchor :entity.app/comment :entity.app/rating module request))

;==================== POSTs ====================
(defn post-feed-filter [user-id db]
  (let [user-id (or user-id :sys.users/guest)
        post-set #{(d/entid db :entity.app/post) :entity.app/post}
        feed-set (->>
                   (d/q
                     '[:find
                       ?e
                       :in $ % ?u
                       :where
                       [?e :sys/entity :entity.app/post]
                       (post-feed ?u ?e)]
                     db
                     '[[(post-feed ?u ?e)
                        [?e :post/hub ?h]
                        [?h :sys/entity :entity.app/hub]
                        [?sub :sys/anchor ?h]
                        [?sub :sys/entity :entity.app/sub]
                        [?sub :sys/author ?u]]
                       [(post-feed ?u ?e)
                        [?e :sys/author ?author]
                        [?sub :sys/anchor ?author]
                        [?sub :sys/entity :entity.app/sub]
                        [?sub :sys/author ?u]]]
                     user-id)
                   (mapcat identity) set)]
    (fn [plaindb ^Datom datom]
      (let [id (.e datom)
            entity (d/entity plaindb id)]
        (if (post-set (:sys/entity entity))
          (some? (feed-set id))
          true)))))

(defn post-favorities-filter [user-id db]
  (let [user-id (or user-id :sys.users/guest)
        post-set #{(d/entid db :entity.app/post) :entity.app/post}
        fav-set (->>
                  (d/q
                    '[:find
                      ?e
                      :in $ ?u
                      :where
                      [?e :sys/entity :entity.app/post]
                      [?fav :sys/anchor ?e]
                      [?fav :sys/entity :entity.app/favorite]
                      [?fav :sys/author ?u]]
                    db user-id)
                  (mapcat identity) set)]
    (fn [plaindb ^Datom datom]
      (let [id (.e datom)
            entity (d/entity plaindb id)]
        (if (post-set (:sys/entity entity))
          (some? (fav-set id))
          true)))))

(defn handle-browse-posts
  [module request]
  (let [{:keys [core data-provider]} module
        request (update
                  request :order
                  (fn [order]
                    (if (= :best (:browse-mode request))
                      [:app/ratings :app/favorites :post/views :sys/date :db/id]
                      order)))]
    (api/respond-success
      core request
      (posts/get-posts
        (browsable-db
          data-provider request
          (partial db/exclude-attr-filter-ctor :post/undercut)
          (when (= :feed (:browse-mode request))
            (partial post-feed-filter (auth-result request :user-id)))
          (when (= :favorities (:browse-mode request))
            (partial post-favorities-filter (or (:favorities-author request)
                                                (auth-result request :user-id)))))
        (->
          request
          (assoc-in [:filter :sys/access-category]
                    (mapv :db/id
                          (auth-result request :permitted-categories)))
          (assoc :short-body true))
        {:value-field :value}))))

(defn handle-read-post
  [module request]
  (let [{:keys [core data-provider]} module
        permitted-action (auth-result request :permitted-action :db/ident)
        db (browsable-db
             data-provider request
             (when (not= :action.app/read-post permitted-action)
               (partial db/exclude-attr-filter-ctor :post/undercut)))
        result (assoc-in (posts/get-post db request)
                         [:auth-result :permitted-action]
                         permitted-action)]
    (api/respond-success core request result)
    (when-not (:disable-counters? request)
      (api/pub-event core {:qn      :event.app/post-read
                           :db/id   (:db/id result)
                           :user-id (auth-result request :user-id)}))))

(defn handle-rate-post [module request]
  (handle-anchor :entity.app/post :entity.app/rating module request))

(defn handle-favorite-post [module request]
  (handle-anchor :entity.app/post :entity.app/favorite module request))

(defn handle-create-post
  [module request]
  (let [{:keys [core data-provider]} module]
    (api/respond-success
      core request
      (posts/upsert-post
        (api/get-conn data-provider)
        (auth-result request :user-id)
        (:value request)
        :create
        (:dry-run? request)))))

(defn handle-edit-post
  [module request]
  (let [{:keys [core data-provider]} module]
    (api/respond-success
      core request
      (posts/upsert-post
        (api/get-conn data-provider)
        (auth-result request :user-id)
        (:value request)
        :update
        (:dry-run? request)))))

;==================== HUBs ====================
(defn hub-feed-filter [user-id db]
  (let [user-id (or user-id :sys.users/guest)
        hub-set #{(d/entid db :entity.app/hub) :entity.app/hub}
        feed-set (->>
                   (d/q
                     '[:find
                       ?h
                       :in $ ?u
                       :where
                       [?h :sys/entity :entity.app/hub]
                       [?sub :sys/anchor ?h]
                       [?sub :sys/entity :entity.app/sub]
                       [?sub :sys/author ?u]]
                     db user-id)
                   (mapcat identity) set)]
    (fn [plaindb ^Datom datom]
      (let [id (.e datom)
            entity (d/entity plaindb id)]
        (if (hub-set (:sys/entity entity))
          (some? (feed-set id))
          true)))))

(defn handle-browse-hubs
  [module request]
  (let [{:keys [core data-provider]} module
        request (update
                  request :order
                  (fn [order]
                    (if (= :best (:browse-mode request))
                      [:app/subs :sys/date :db/id]
                      order)))]
    (api/respond-success
      core
      request
      (hubs/get-hubs
        (browsable-db
          data-provider request
          (when (= :feed (:browse-mode request))
            (partial hub-feed-filter (auth-result request :user-id))))
        (assoc-in
          request
          [:filter :sys/access-category]
          (mapv :db/id (auth-result request :permitted-categories)))
        {:value-field :value}))))

(defn handle-read-hub
  [module request]
  (let [{:keys [core data-provider]} module
        db (browsable-db data-provider request)]
    (api/respond-success
      core
      request
      (hubs/get-hub db request))))

(defn handle-join-hub [module request]
  (handle-anchor :entity.app/hub :entity.app/sub module request))

;==================== USERs ====================
(defn user-feed-filter [user-id db]
  (let [user-id (or user-id :sys.users/guest)
        user-set #{(d/entid db :entity.sys/user) :entity.sys/user}
        feed-set (->>
                   (d/q
                     '[:find
                       ?user
                       :in $ ?u
                       :where
                       [?user :sys/entity :entity.sys/user]
                       [?sub :sys/anchor ?user]
                       [?sub :sys/entity :entity.app/sub]
                       [?sub :sys/author ?u]]
                     db user-id)
                   (mapcat identity) set)]
    (fn [plaindb ^Datom datom]
      (let [id (.e datom)
            entity (d/entity plaindb id)]
        (if (user-set (:sys/entity entity))
          (some? (feed-set id))
          true)))))

(defn user-sys-filter [db]
  (let [user-set #{(d/entid db :entity.sys/user) :entity.sys/user}]
    (fn [plaindb ^Datom datom]
      (let [entity (d/entity plaindb (.e datom))]
        (if (user-set (:sys/entity entity))
          (not (:user/is-sys entity))
          true)))))

(defn user-subscribers-filter [user-id db]
  (let [user-id (or user-id :sys.users/guest)
        user-set #{(d/entid db :entity.sys/user) :entity.sys/user}
        sub-set (->>
                  (d/q
                    '[:find
                      ?u
                      :in $ ?e
                      :where
                      [?u :sys/entity :entity.sys/user]
                      [?sub :sys/anchor ?e]
                      [?sub :sys/author ?u]
                      [?sub :sys/entity :entity.app/sub]]
                    db user-id)
                  (mapcat identity) set)]
    (fn [plaindb ^Datom datom]
      (let [id (.e datom)
            entity (d/entity plaindb id)]
        (if (user-set (:sys/entity entity))
          (some? (sub-set id))
          true)))))

(defn handle-browse-users
  [module request]
  (let [{:keys [core data-provider]} module
        request (update
                  request :order
                  (fn [order]
                    (if (= :best (:browse-mode request))
                      [:app/subs :sys/date :db/id]
                      order)))]
    (api/respond-success
      core
      request
      (users/get-users
        (browsable-db
          data-provider request
          user-sys-filter
          (when (= :feed (:browse-mode request))
            (partial user-feed-filter (auth-result request :user-id)))
          (when (= :subscribers (:browse-mode request))
            (partial user-subscribers-filter
                     (or (:sub-anchor request)
                         (auth-result request :user-id)))))
        (assoc-in
          request
          [:filter :sys/access-category]
          (mapv :db/id (auth-result request :permitted-categories)))
        {:value-field :value}))))

(defn handle-read-user
  [module request]
  (let [{:keys [core data-provider]} module
        db (browsable-db data-provider request user-sys-filter)]
    (api/respond-success
      core request
      (users/get-user db request))))

(defn handle-subscribe-user [module request]
  (handle-anchor :entity.sys/user :entity.app/sub module request))

(defn handle-change-profile
  [module request]
  (let [{:keys [core]} module
        user-id (auth-result request :user-id)]
    (async/pipe
      (api/call-async
        core
        (-> request
            (select-keys [:user/about])
            (assoc
              :qn :user-directory/update-user
              :sys/author user-id
              :db/id user-id)
            (with-meta {})))
      (api/get-response-chan core request))))

(defn handle-change-credentials
  [module request]
  (let [{:keys [core]} module
        user-id (auth-result request :user-id)]
    (async/pipe
      (api/call-async
        core
        (-> request
            (select-keys [:user/login :user/login-current :user/pwd :user/pwd-current])
            (assoc
              :qn :user-directory/update-user-credentials
              :sys/author user-id
              :db/id user-id)
            (with-meta {})))
      (api/get-response-chan core request))))

;==================== TRACKER ====================

(defn tracker-filter [entity-types user-id db]
  (let [user-id (or user-id :sys.users/guest)
        eset (set entity-types)
        tracker-set (->>
                      (d/q
                        '[:find
                          ?e
                          :in $ % ?u [?ents ...]
                          :where
                          [?e :sys/entity ?ents]
                          (user-anchored ?u ?e)
                          ]
                        db
                        '[[(user-anchored ?u ?e)
                           [?e :sys/anchor ?anchor]
                           [(identity ?u) ?anchor]]
                          [(user-anchored ?u ?e)
                           [?e :sys/anchor ?anchor]
                           [?anchor :sys/author ?u]]
                          [(user-anchored ?u ?e)
                           [?e :sys/anchor ?anchor]
                           [(datomic.api/invoke $ :app/root-anchor-fn $ ?e) ?root-anchor]
                           [?root-anchor :sys/author ?u]]
                          [(user-anchored ?u ?e)
                           [?e :sys/anchor ?anchor]
                           [(datomic.api/invoke $ :app/root-anchor-fn $ ?e) ?root-anchor]
                           [(identity ?u) ?root-anchor]]
                          [(user-anchored ?u ?e)
                           [?e :sys/author ?u]
                           [?ev :sys/anchor ?e]]
                          [(user-anchored ?u ?e)
                           (not [?e :sys/author ?u])
                           [?ev :sys/anchor ?e]
                           (user-anchored ?u ?ev)
                           ]]
                        user-id (vec entity-types))
                      (mapcat identity) set)]
    (fn [plaindb ^Datom datom]
      (let [id (.e datom)
            entity (d/entity plaindb id)]
        (if (eset (:sys/entity entity))
          (some? (tracker-set id))
          true)))))

(defn handle-read-user-tracker
  [module request]
  (let [{:keys [core data-provider]} module
        entity-set #{:entity.app/comment :entity.app/sub :entity.app/rating :entity.app/favorite}
        entity (case (:browse-mode request)
                 :comments :entity.app/comment
                 :subscribers :entity.app/sub
                 :ratings :entity.app/rating
                 entity-set)
        user-id (or (:user-id request) (auth-result request :user-id) :sys.users/guest)]
    (api/respond-success
      core
      request
      (->
        (anch/get-anchored-vals
          (filtered-db
            data-provider
            [(partial tracker-filter entity-set user-id)])
          (assoc request
            :pattern [:db/id
                      :sys/date
                      :rating/value
                      :comment/body
                      {:sys/entity [:db/id :db/ident]}
                      {:sys/anchor [:db/id
                                    :post/title
                                    :comment/body
                                    {:sys/entity [:db/id :db/ident]}
                                    {:sys/anchor '...}]}
                      {:sys/author [:db/id :user/nickname]}
                      :sys/read-by]
            :entity entity
            :filter {:sys/author {:v user-id :op :not}})
          {:value-field :value})
        (update
          :value
          (fn [items]
            (mapv #(update % :sys/read-by (fn [flags] (some (fn [f] (= user-id (:db/id f))) flags)))
                  items)))))))

;==================== App-events ====================
(defn notify-anchor-changed [core db anchor-id]
  (api/pub-event
    core
    {:qn          :notifications.events/entity-changed
     :entity      (d/pull
                    db
                    [:db/id
                     {:sys/entity [:db/id :db/ident]}
                     :app/favorites :app/ratings :app/subs :app/comments :post/views]
                    anchor-id)
     :anchor-path (db/anchor-path db anchor-id)
     :target      :notifier/subscribers}))

(defn process-profile-event [core db event-data]
  (let [{:keys [data]} event-data
        anchor
        (d/pull db
                [:db/id
                 :post/title
                 :comment/body
                 {:sys/entity [:db/id :db/ident]}
                 {:sys/author [:db/id :user/nickname]}
                 {:sys/anchor '...}]
                (get-in data [:sys/anchor :db/id]))
        author (get-in data [:sys/author :db/id])
        event-data (assoc data :sys/anchor anchor)
        addressees (->>
                     anchor
                     (iterate :sys/anchor)
                     (take-while some?)
                     (map (fn [anchor] (get-in anchor [:sys/author :db/id])))
                     (remove #{author})
                     set)]
    (when (seq addressees)
      (api/pub-event
        core
        {:qn         :notifications.events/profile-event
         :data       event-data
         :addressees addressees
         :target     :notifier/subscribers}))))

(defn handle-app-event [module event]
  (let [{:keys [core data-provider]} module
        conn (api/get-conn data-provider)
        {:keys [qn user-id]} event]

    (case qn

      (:event.app/anchored-added :event.app/anchored-retracted)
      (let [db (d/db conn)]
        (notify-anchor-changed core db (:anchor-id event))
        (when (= :event.app/anchored-added qn)
          (process-profile-event core db {:data (:anchored event)})))

      (:event.app/comment-added :event.app/comment-changed)
      (let [db (d/db conn)
            comment (:comment event)
            anchor-path (db/anchor-path db (:db/id comment))]
        (when (= :event.app/comment-added qn)
          (notify-anchor-changed core db (first anchor-path))
          (process-profile-event core db {:data comment}))
        (api/pub-event
          core
          {:qn          (if (= :event.app/comment-added qn)
                          :notifications.events/entity-added
                          :notifications.events/entity-changed)
           :entity      (:comment event)
           :anchor-path anchor-path
           :target      :notifier/subscribers}))

      :event.app/post-read
      (let [dbid (:db/id event)
            tx-result @(d/transact
                         conn
                         [[:app/swap+ dbid :post/views nil nil]
                          [:db/add #db/id[:db.part/tx]
                           :sys/author (or user-id :sys.users/guest)]])
            db (:db-after tx-result)
            views (:post/views (d/entity db dbid) 0)]
        (when (= 0 (mod views (api/get-conf module :post-views-notification-limit 10)))
          (notify-anchor-changed core db dbid)))

      :event.app/tracker-event-read
      (when-let [user-id (auth-result event :user-id)]
        (let [dbid (:event-id event)]
          (d/transact
            conn
            [[:db/add dbid :sys/read-by user-id]
             [:db/add #db/id[:db.part/tx] :sys/author user-id]])))
      nil)))

;==================== Administrative actions ====================

(defn serve-access-category-search [module request]
  (let [{:keys [core data-provider]} module
        params
        (merge
          (select-keys
            request [:search-string :filter :pattern :order :limit :offset])
          {:entity        :entity.sys/access-category
           :search-fields (:search-fields request [:db/doc])})]
    (api/respond-success
      core request (db/fetch-data (api/get-db data-provider) params))))

(defn serve-access-level-search [module request]
  (let [{:keys [core data-provider]} module
        params
        (merge
          (select-keys
            request [:search-string :filter :pattern :order :limit :offset])
          {:entity        :entity.sys/access-level
           :search-fields (:search-fields request [:access-level/name :db/doc])})]
    (api/respond-success
      core request (db/fetch-data (api/get-db data-provider) params))))

(defn serve-adm-browse-users [module request]
  (let [{:keys [core]} module]
    (api/call-async
      core (assoc
             (with-meta request (dissoc (meta request) :auth-result :remote?))
             :qn :user-directory/browse-users))))

(defn serve-adm-read-user [module request]
  (let [{:keys [core]} module]
    (api/call-async
      core (assoc
             (with-meta request (dissoc (meta request) :auth-result :remote?))
             :qn :user-directory/read-user))))

(defn check-access-level [module access-level-id-or-ident]
  (let [{:keys [data-provider]} module
        db (api/get-db data-provider)]
    (when access-level-id-or-ident
      (let [ual-entity (d/entity db access-level-id-or-ident)]
        (when-not (:access-level/assignable? ual-entity)
          (throw (ex-info "Unsupported access level used" {:access-level access-level-id-or-ident})))
        ual-entity))))

(defn serve-adm-create-user [module request]
  (let [{:keys [core]} module
        ual-entity (check-access-level module (:user/access-level request))
        request (->
                  request
                  (assoc :sys/access-category :access-category/administrative)
                  (tools/->if (:access-level/for-sale ual-entity)
                              assoc :sys/access-category :access-category/open))]
    (api/call-async
      core
      (api/set-response-chan
        core
        (assoc
          (:value request)
          :qn :user-directory/create-user
          :sys/author (auth-result request :user-id))
        (api/get-response-chan core request)))))

(defn serve-adm-update-user [module request]
  (let [{:keys [core]} module]
    (api/call-async
      core
      (api/set-response-chan
        core
        (assoc
          (:value request)
          :qn :user-directory/update-user
          :sys/author (auth-result request :user-id))
        (api/get-response-chan core request)))))

(def adm-hub-pattern
  [:*
   {:sys/entity [:db/id :db/ident]}
   {:sys/author [:db/id :user/nickname]}
   {:sys/access-category [:db/id :db/ident :db/doc]}])

(defn serve-adm-browse-hubs [module request]
  (let [{:keys [core data-provider]} module
        db (api/get-db-filtered data-provider)]
    (api/respond-success
      core request (hubs/get-hubs db (assoc request :pattern adm-hub-pattern) {:value-field :value}))))

(defn serve-adm-read-hub [module request]
  (let [{:keys [core data-provider]} module
        db (api/get-db-filtered data-provider)]
    (api/respond-success
      core request (hubs/get-hub db (assoc request :pattern adm-hub-pattern)))))

(defn serve-adm-create-hub [module request]
  (let [{:keys [core data-provider]} module]
    (api/respond-success
      core request
      (hubs/upsert-hub
        (api/get-conn data-provider)
        (auth-result request :user-id)
        (:value request)
        :create
        (:dry-run? request)))))

(defn serve-adm-update-hub [module request]
  (let [{:keys [core data-provider]} module]
    (api/respond-success
      core request
      (hubs/upsert-hub
        (api/get-conn data-provider)
        (auth-result request :user-id)
        (:value request)
        :update
        (:dry-run? request)))))

(defn serve-adm-remove-hub [module request]
  (api/respond-success (:core module) request))

(defn serve-adm-billing-browse-orders [module request]
  (let [{:keys [core]} module]
    (api/call-async
      core (assoc
             (with-meta request (dissoc (meta request) :auth-result :remote?))
             :qn :billing.adm/browse-orders))))

(defn serve-adm-billing-read-order [module request]
  (let [{:keys [core]} module]
    (api/call-async
      core (assoc
             (with-meta request (dissoc (meta request) :auth-result :remote?))
             :qn :billing.adm/read-order))))

(defn serve-adm-billing-get-balance [module request]
  (let [{:keys [core]} module]
    (api/call-async
      core (assoc
             (with-meta request (dissoc (meta request) :auth-result :remote?))
             :qn :btc-gate/get-balance))))

(defn serve-adm-billing-force-forwarding [module request]
  (let [{:keys [core]} module]
    (api/call-async
      core (assoc
             (with-meta request (dissoc (meta request) :auth-result :remote?))
             :qn :btc-gate/force-forwarding))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DECLARATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ring-handler [app request]
  (let [handler (:spa-handler app)]
    (handler request)))

(defn action-browse-dtls [& [deault-mode]]
  [:dtlrules
   [:browse-mode [{:p [:params :browse-mode] :or (or deault-mode :all)}]]
   [:limit [[:params :limit]]]
   [:offset [[:params :offset]]]
   [:search-string [[:params :search-string]]]])

(def uri-browse-dtls
  [:dtlrules
   ["%s/" [:browse-mode]]
   ["%s" [:dtlrules {:? "?" :& "&"}
          ["limit=%s" [:limit]]
          ["offset=%s" [:offset]]
          ["search-string=%s"
           [:search-string]]]]])

(def action-dbid-dtls
  [:dtlrules
   [:db/id [[:params :id]]]])

(defn routes [& route-descs]
  (mapv
    (fn [route-desc]
      (let [spec (peek route-desc)]
        (conj (pop route-desc) (assoc spec :handler ring-handler))))
    route-descs))

(defrecord App []
  api/IModule
  api/IComponent
  (start [app] app)
  (stop [app] app))

(def post-tag-schema
  [[:data/type :data-type/vector]
   [:seq/count {:min 0 :max 10}]
   [:seq/each
    [:data/alternative
     [[:map/spec-closed
       {:tag/title [[:data/type :data-type/string]
                    [:string/not-empty]
                    [:string/length {:min 3}]]}]]
     [[:map/spec-closed
       {:db/id db/id-required-schema}]]]]])

(def post-hub-schema
  [[:data/type :data-type/vector]
   [:seq/count {:min 1 :max 3}]
   [:seq/each
    [:data/alternative
     [[:map/spec-closed
       {:db/id db/id-required-schema}]]
     db/id-required-schema]]])

(def update-post-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value
     [:map/spec-closed
      {:db/id               db/id-required-schema
       :post/title          db/non-empty-string-shema
       :post/preview        db/non-empty-string-shema
       :post/undercut       [[:data/type :data-type/string]]
       :sys/access-category [db/id-schema]
       :post/hub            post-hub-schema
       :post/tag            post-tag-schema}]}]])

(def create-post-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value
     [:map/spec-closed
      {:post/title          db/non-empty-string-required-shema
       :post/preview        db/non-empty-string-required-shema
       :post/undercut       [[:data/type :data-type/string]]
       :sys/access-category db/id-required-schema
       :post/hub            (db/require-schema post-hub-schema)
       :post/tag            post-tag-schema}]}]])

(def write-comment-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value [:map/spec-closed
             {:comment/body db/non-empty-string-required-shema
              :sys/anchor   db/id-required-schema}]}]])

(def edit-comment-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value [:map/spec-closed
             {:db/id        db/id-required-schema
              :comment/body db/non-empty-string-required-shema}]}]])

(def unanchor-schema
  [[:data/type :data-type/map]
   [:map/spec-closed
    {:retraction? [[:data/requirement :data-requirement/required]
                   [:data/type :data-type/boolean]]
     :value       [[:map/spec-closed
                    {:db/id db/id-required-schema}]]}]])

(def rate-schema
  [[:data/alternative
    [[:data/type :data-type/map]
     [:map/spec-closed
      {:value [:map/spec-closed
               {:sys/anchor   db/id-schema
                :rating/value [[:data/requirement :data-requirement/required]
                               [:data/type :data-type/long]]}]}]]
    unanchor-schema]])

(def generic-anchor-schema
  [[:data/alternative
    [[:data/type :data-type/map]
     [:map/spec-closed
      {:value [:map/spec-closed
               {:sys/anchor db/id-schema}]}]]
    unanchor-schema]])

;==================== Administrative schemas ====================
(def create-hub-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value
     [:map/spec-closed
      {:hub/name            db/non-empty-string-required-shema
       :hub/description     db/non-empty-string-required-shema
       :sys/access-category db/id-required-schema}]}]])

(def update-hub-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value
     [:map/spec-closed
      {:db/id               db/id-required-schema
       :hub/name            db/non-empty-string-shema
       :hub/description     db/non-empty-string-shema
       :sys/access-category db/id-schema}]}]])

(def create-user-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value
     [:map/spec-closed
      {:user/login         (sch/required users/login-schema)
       :user/pwd           (sch/required users/pwd-schema)
       :user/nickname      (sch/required users/nickname-schema)
       :user/access-level  (sch/required users/long-or-kw-schema)
       :user/access-expiry [[:data/type :data-type/instant]]
       :user/about         db/non-empty-string-shema}]}]])

(def update-user-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value
     [:map/spec-closed
      {:db/id              db/id-required-schema
       :user/login         users/login-schema
       :user/pwd           users/pwd-schema
       :user/nickname      users/nickname-schema
       :user/access-level  users/long-or-kw-schema
       :user/access-expiry [[:data/type :data-type/instant]]
       :user/about         db/non-empty-string-shema}]}]])

(defn new-app [& [m]]
  (map->App
    (merge
      {api/id-key
       :app
       api/static-deps-key
       [:data-provider :spa-handler]
       api/event-subs-key
       [{:msg-filter  {:nsqn :event.app}
         :handler     #'handle-app-event
         :parallelism {:type :thread}}]
       api/request-servers-key
       [{:msg-filter      :app/hub-search
         :handler         #'handle-hub-search
         :validation-opts {:schema db/search-schema}}

        {:msg-filter      :app/tag-search
         :handler         #'handle-tag-search
         :validation-opts {:schema db/search-schema}}

        {:msg-filter         :action.app/browse-posts
         :parallelism        {:n 8}
         :handler            #'handle-browse-posts
         :validation-opts    {:schema db/search-schema}
         :authorization-opts {:check-permission :action.app/read-post}}

        {:msg-filter         :action.app/read-post
         :handler            #'handle-read-post
         :validation-opts    {:schema db/read-schema}
         :authorization-opts {:category-search-root [:filter :db/id]}}

        {:msg-filter         :action.app/create-post
         :handler            #'handle-create-post
         :validation-opts    {:schema create-post-schema
                              :scope  #{:client}}
         :authorization-opts {:category-search-root [:value :sys/access-category]}}

        {:msg-filter         :action.app/edit-post
         :handler            #'handle-edit-post
         :validation-opts    {:schema update-post-schema
                              :scope  #{:client}}
         :authorization-opts {:category-search-root [:value :db/id]
                              :rule-opts            {:brule.app/authorship
                                                     {:rule-root [:value :db/id]}}}}

        {:msg-filter         :action.app/read-post-discussion
         :handler            #'handle-read-post-discussion
         :authorization-opts {:category-search-root [:filter :sys/anchor]}}

        {:msg-filter         :action.app/browse-comments
         :handler            #'handle-browse-comments
         :validation-opts    {:schema db/search-schema}
         :authorization-opts {:check-permission :action.app/read-post-discussion}}

        {:msg-filter         :action.app/write-comment
         :handler            #'handle-write-comment
         :validation-opts    {:schema write-comment-schema
                              :scope  #{:client}}
         :authorization-opts {:category-search-root [:value :sys/anchor]}}

        {:msg-filter         :action.app/edit-comment
         :handler            #'handle-edit-comment
         :validation-opts    {:schema edit-comment-schema
                              :scope  #{:client}}
         :authorization-opts {:category-search-root [:value :db/id]
                              :rule-opts            {:brule.app/authorship
                                                     {:rule-root [:value :db/id]}}}}

        {:msg-filter         :action.app/rate-post
         :handler            #'handle-rate-post
         :validation-opts    {:schema rate-schema}
         :authorization-opts {:category-search-root     [:value :sys/anchor]
                              :category-search-root-alt [:value :db/id]}}

        {:msg-filter         :action.app/favorite-post
         :handler            #'handle-favorite-post
         :validation-opts    {:schema generic-anchor-schema}
         :authorization-opts {:category-search-root     [:value :sys/anchor]
                              :category-search-root-alt [:value :db/id]}}

        {:msg-filter         :action.app/rate-comment
         :handler            #'handle-rate-comment
         :validation-opts    {:schema rate-schema}
         :authorization-opts {:category-search-root     [:value :sys/anchor]
                              :category-search-root-alt [:value :db/id]}}

        {:msg-filter         :action.app/browse-hubs
         :handler            #'handle-browse-hubs
         :validation-opts    {:schema db/search-schema}
         :authorization-opts {:check-permission :action.app/read-hub}}

        {:msg-filter         :action.app/read-hub
         :handler            #'handle-read-hub
         :validation-opts    {:schema db/read-schema}
         :authorization-opts {:category-search-root [:filter :db/id]}}

        {:msg-filter         :action.app/join-hub
         :handler            handle-join-hub
         :validation-opts    {:schema generic-anchor-schema}
         :authorization-opts {:category-search-root     [:value :sys/anchor]
                              :category-search-root-alt [:value :db/id]}}

        {:msg-filter         :action.app/browse-user-profiles
         :handler            #'handle-browse-users
         :validation-opts    {:schema db/search-schema}
         :authorization-opts {:check-permission :action.app/read-user-profile}}

        {:msg-filter         :action.app/read-user-profile
         :handler            #'handle-read-user
         :validation-opts    {:schema db/search-schema}
         :authorization-opts {:category-search-root [:filter :db/id]
                              :rule-opts            {:brule.app/authorship
                                                     {:rule-root [:filter :db/id]}}}}

        {:msg-filter         :action.app/read-user-tracker
         :handler            #'handle-read-user-tracker
         :validation-opts    {:schema db/search-schema}
         :authorization-opts {:category-search-root :access-category/administrative
                              :rule-opts            {:brule.app/authorship
                                                     {:rule-root [:user-id]}}}}

        {:msg-filter         :action.app/subscribe-user
         :handler            #'handle-subscribe-user
         :validation-opts    {:schema generic-anchor-schema}
         :authorization-opts {:category-search-root     [:value :sys/anchor]
                              :category-search-root-alt [:value :db/id]}}

        {:msg-filter :app/change-profile
         :handler    #'handle-change-profile}
        {:msg-filter :app/change-credentials
         :handler    #'handle-change-credentials}

        ;==================== Administrative actions ====================
        {:msg-filter         :action.adm/access-category-search
         :handler            #'serve-access-category-search
         :validation-opts    {:schema db/search-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/access-level-search
         :handler            #'serve-access-level-search
         :validation-opts    {:schema db/search-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/browse-users
         :handler            #'serve-adm-browse-users
         :validation-opts    {:schema db/search-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/read-user
         :handler            #'serve-adm-read-user
         :validation-opts    {:schema db/read-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/create-user
         :handler            #'serve-adm-create-user
         :validation-opts    {:schema create-user-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/update-user
         :handler            #'serve-adm-update-user
         :validation-opts    {:schema update-user-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/browse-hubs
         :handler            #'serve-adm-browse-hubs
         :validation-opts    {:schema db/search-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/read-hub
         :handler            #'serve-adm-read-hub
         :validation-opts    {:schema db/read-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/create-hub
         :handler            #'serve-adm-create-hub
         :validation-opts    {:schema create-hub-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/update-hub
         :handler            #'serve-adm-update-hub
         :validation-opts    {:schema update-hub-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/remove-hub
         :handler            #'serve-adm-remove-hub
         :validation-opts    {:schema db/read-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/billing-browse-orders
         :handler            #'serve-adm-billing-browse-orders
         :validation-opts    {:schema db/search-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/billing-read-order
         :handler            #'serve-adm-billing-read-order
         :validation-opts    {:schema db/read-schema}
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/billing-get-balance
         :handler            #'serve-adm-billing-get-balance
         :authorization-opts {:category-search-root :access-category/administrative}}
        {:msg-filter         :action.adm/billing-force-forwarding
         :handler            #'serve-adm-billing-force-forwarding
         :authorization-opts {:category-search-root :access-category/administrative}}
        ]
       api/routes-key
       (routes
         ["/" :get
          {:action-qn :spa/sign-up}]

         ["/sign-up/" :get
          {:action-qn :spa/sign-up
           :uri-rule  "/sign-up/"}]

         ["/sign-in/" :get
          {:action-qn :spa/sign-in
           :uri-rule  "/sign-in/"}]

         ["/posts/" :get ""
          {:action-qn   :action.app/browse-posts
           :action-rule (action-browse-dtls)
           :uri-rule    ["/posts/%s" uri-browse-dtls]}]

         ["/posts/" :get "create/"
          {:action-qn :action.app/create-post
           :uri-rule  "/posts/create/"}]

         ["/posts/" :get [[keyword :browse-mode] "/"]
          {:action-qn   :action.app/browse-posts
           :action-rule (action-browse-dtls)
           :uri-rule    ["/posts/%s" uri-browse-dtls]}]

         ["/posts/" :get [[long :id] "/"]
          {:action-qn   :action.app/read-post
           :action-rule [:filter action-dbid-dtls]
           :uri-rule    ["/posts/%s/" [[:filter :db/id]]]}]

         ["/posts/" :get [[long :id] "/edit/"]
          {:action-qn   :action.app/edit-post
           :action-rule [:filter action-dbid-dtls]
           :uri-rule    ["/posts/%s/edit/" [[:filter :db/id]]]}]

         ["/hubs/" :get ""
          {:action-qn   :action.app/browse-hubs
           :action-rule (action-browse-dtls)
           :uri-rule    ["/hubs/%s" uri-browse-dtls]}]

         ["/hubs/" :get [[keyword :browse-mode] "/"]
          {:action-qn   :action.app/browse-hubs
           :action-rule (action-browse-dtls)
           :uri-rule    ["/hubs/%s" uri-browse-dtls]}]

         ["/hubs/" :get "create/"
          {:action-qn :action.app/create-hub
           :uri-rule  "/hubs/create/"}]

         ["/hubs/" :get [[long :id] "/"]
          {:action-qn   :action.app/read-hub
           :action-rule (conj (action-browse-dtls) [:filter action-dbid-dtls])
           :uri-rule    [:dtlrules
                         ["/hubs/%s/" [[:filter :db/id]]]
                         ["%s" uri-browse-dtls]]}]

         ["/hubs/" :get [[long :id] "/" [keyword :browse-mode] "/"]
          {:action-qn   :action.app/read-hub
           :action-rule (conj (action-browse-dtls) [:filter action-dbid-dtls])
           :uri-rule    [:dtlrules
                         ["/hubs/%s/" [[:filter :db/id]]]
                         ["%s" uri-browse-dtls]]}]

         ["/hubs/" :get [:id "/edit/"]
          {:action-qn   :action.app/edit-hub
           :action-rule [:filter action-dbid-dtls]
           :uri-rule    ["/posts/%s/edit/" [[:filter :db/id]]]}]

         ["/my/profile/" :get ""
          {:action-qn   :action.app/my-profile
           :action-rule []
           :uri-rule    ["/my/profile/" []]}]

         ["/users/" :get ""
          {:action-qn   :action.app/browse-user-profiles
           :action-rule (action-browse-dtls)
           :uri-rule    ["/users/%s" uri-browse-dtls]}]

         ["/users/" :get [[keyword :browse-mode] "/"]
          {:action-qn   :action.app/browse-user-profiles
           :action-rule (action-browse-dtls)
           :uri-rule    ["/users/%s" uri-browse-dtls]}]

         ["/users/" :get [[long :id] "/"]
          {:action-qn   :action.app/read-user-profile
           :action-rule (conj (action-browse-dtls :profile) [:filter action-dbid-dtls])
           :uri-rule    [:dtlrules
                         ["/users/%s/" [[:filter :db/id]]]
                         ["%s" uri-browse-dtls]]}]

         ["/users/" :get [[long :id] "/" [keyword :browse-mode] "/"]
          {:action-qn   :action.app/read-user-profile
           :action-rule (conj (action-browse-dtls :profile) [:filter action-dbid-dtls])
           :uri-rule    [:dtlrules
                         ["/users/%s/" [[:filter :db/id]]]
                         ["%s" uri-browse-dtls]]}]
         ["/tracker/" :get
          {:action-qn   :action.app/read-user-tracker
           :action-rule (action-browse-dtls)
           :uri-rule    [:dtlrules
                         ["/tracker/"]
                         ["%s" uri-browse-dtls]]}]
         ["/tracker/" :get [[keyword :browse-mode] "/"]
          {:action-qn   :action.app/read-user-tracker
           :action-rule (action-browse-dtls)
           :uri-rule    [:dtlrules
                         ["/tracker/"]
                         ["%s" uri-browse-dtls]]}]

         ;==================== Administrative routes ====================
         ["/admin/users/" :get ""
          {:action-qn   :action.adm/users
           :action-rule (action-browse-dtls :search)
           :uri-rule    ["/admin/users/%s" uri-browse-dtls]}]

         ["/admin/users/" :get [[keyword :browse-mode] "/"]
          {:action-qn   :action.adm/users
           :action-rule (action-browse-dtls :search)
           :uri-rule    ["/admin/users/%s" uri-browse-dtls]}]

         ["/admin/users/" :get [[long :id] "/"]
          {:action-qn   :action.adm/update-user
           :action-rule [:filter action-dbid-dtls]
           :uri-rule    ["/admin/users/%s/" [[:filter :db/id]]]}]

         ["/admin/hubs/" :get ""
          {:action-qn   :action.adm/hubs
           :action-rule (action-browse-dtls :search)
           :uri-rule    ["/admin/hubs/%s" uri-browse-dtls]}]

         ["/admin/hubs/" :get [[keyword :browse-mode] "/"]
          {:action-qn   :action.adm/hubs
           :action-rule (action-browse-dtls :search)
           :uri-rule    ["/admin/hubs/%s" uri-browse-dtls]}]

         ["/admin/hubs/" :get [[long :id] "/"]
          {:action-qn   :action.adm/update-hub
           :action-rule (action-browse-dtls :search)
           :uri-rule    ["/admin/hubs/%s/" [[:filter :db/id]]]}]

         ["/admin/billing/" :get ""
          {:action-qn   :action.adm/billing
           :action-rule (action-browse-dtls :billing-orders)
           :uri-rule    ["/admin/billing/%s" uri-browse-dtls]}]

         ["/admin/billing/" :get [[keyword :browse-mode] "/"]
          {:action-qn   :action.adm/billing
           :action-rule (action-browse-dtls :billing-orders)
           :uri-rule    ["/admin/billing/%s" uri-browse-dtls]}]

         )}
      m)))



