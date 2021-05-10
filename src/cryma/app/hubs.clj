(ns cryma.app.hubs
  (:require
    [cryma.app.db :as db]
    [datomic.api :as d]
    [cryma.core.tools :as tools]))

(defn get-hubs [db request & [with-info?]]
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
                      {:sys/_anchor [:*
                                     {:sys/entity [:db/id :db/ident]}
                                     {:sys/author [:db/id :user/nickname]}]}]))
              (update
                :search-fields
                #(or %
                     [:hub/name :hub/description]))
              (assoc :entity :entity.app/hub))
          with-info?)]
    data))

(defn get-hub [db request]
  (first (get-hubs db request)))

(defn prepare-hub [db user-id hub operation]
  (let [hub (-> hub
                 (select-keys
                   [:db/id :sys/access-category :hub/name :hub/description]))]
    (if (= :create operation)
      (db/prepare-entity-data db hub :entity.app/hub user-id)
      (tools/prepare-value hub))))

(defn upsert-hub
  [conn user-id hub operation & [dry?]]
  (let [db (d/db conn)
        hub (prepare-hub db user-id hub operation)
        tx-fn (case operation
                :create :app/add-entity
                :update :app/update-entity)
        tx-result (if dry?
                    (d/with db [[:app/add-entity hub user-id]])
                    @(d/transact conn [[tx-fn hub user-id]]))
        dbid (db/get-tx-id hub tx-result)]
    (get-hub (:db-after tx-result)
              {:filter {:db/id dbid}})))