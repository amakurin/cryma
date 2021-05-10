(ns cryma.app.posts
  (:require
    [datomic.api :as d]
    [cryma.app.db :as db]
    [cryma.core.tools :as tools]
    [cryma.core.schema :as sch])
  (:import (java.util Date)))

(defn get-posts [db request & [with-info?]]
  (db/fetch-data
    db
    (-> request
        (update
          :pattern
          #(or %
               [:*
                {:sys/author [:db/id :user/nickname]}
                {:sys/entity [:db/id :db/ident]}
                {:sys/access-category [:db/id :db/ident]}
                {:post/hub
                 [:db/id :hub/name :hub/description
                  {:sys/access-category [:db/id :db/ident]}]}
                {:post/tag [:db/id :tag/title]}
                {:sys/_anchor [:*
                               {:sys/entity [:db/id :db/ident]}
                               {:sys/author [:db/id :user/nickname]}]}]
               ))
        (update
          :search-fields
          #(or %
               [:post/preview :post/undercut :post/title
                [:post/tag :tag/title]
                [:post/hub :hub/name]]))
        (update
          :order
          #(or %
               [:sys/date :db/id]))
        (assoc :entity :entity.app/post))
    with-info?))

(defn get-post [db request]
  (first (get-posts db request)))

(defn fix-hubs [{:keys [:post/hub :sys/access-category] :as post} db]
  (if (and (seq hub) access-category)
    (assoc post
      :post/hub
      (mapv first
            (d/q
              '[:find (pull ?e [:db/id])
                :in $ [?e ...] ?ac
                :where
                [?e :sys/entity :entity.app/hub]
                [?e :sys/access-category ?eac]
                [?eac :access-category/weight ?eacw]
                [?ac :access-category/weight ?acw]
                [(>= ?eacw ?acw)]]
              db
              (mapv :db/id hub)
              access-category)))
    post))

(defn prepare-post [db user-id post operation]
  (let [post (-> post
                 (select-keys
                   [:db/id :sys/access-category :post/title
                    :post/preview :post/undercut :post/hub :post/tag])
                 (fix-hubs db))]
    (if (= :create operation)
      (db/prepare-entity-data db post :entity.app/post user-id)
      (tools/prepare-value post))))

(defn upsert-post
  [conn user-id post operation & [dry?]]
  (let [db (d/db conn)
        post (prepare-post db user-id post operation)
        tx-fn (case operation
                :create :app/add-entity
                :update :app/update-entity)
        tx-result (if dry?
                    (d/with db [[:app/add-entity post user-id]])
                    @(d/transact conn [[tx-fn post user-id]]))
        dbid (db/get-tx-id post tx-result)]
    (get-post (:db-after tx-result)
              {:filter {:db/id dbid}})))


