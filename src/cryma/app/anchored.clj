(ns cryma.app.anchored
  (:require [datomic.api :as d]
            [clojure.core.async :refer [<! <!! >! >!! put! chan go go-loop]]
            [cryma.app.db :as db]))

(defn get-anchored-vals [db request & [with-info?]]
  (db/fetch-data
    db
    (-> request
        (update
          :pattern
          #(or %
               [:*
                {:sys/author [:db/id :user/nickname]}
                {:sys/entity [:db/id :db/ident]}]))
        (update
          :search-fields
          #(or %
               [[:comment/body]
                [:sys/author :user/nickname]]))
        (update
          :order
          #(or %
               [:sys/date :db/id])))
    with-info?))

(defn get-anchored-val [db request]
  (first (get-anchored-vals db request)))

(defn create-anchor
  [conn user-id entity-ident anchored-data]
  (let [db (d/db conn)]
    (if-let [dbid (ffirst (d/q '[:find ?e :in $ ?anchor ?entity ?author
                                 :where
                                 [?e :sys/anchor ?anchor]
                                 [?e :sys/author ?author]
                                 [?e :sys/entity ?entity]]
                               db (:sys/anchor anchored-data) entity-ident user-id))]
      (get-anchored-val
        db
        {:entity entity-ident
         :filter {:db/id dbid}})
      (let [data (db/prepare-entity-data db anchored-data entity-ident user-id)
            tx-result @(d/transact conn
                                   [[:app/add-entity data user-id]])
            dbid (db/get-tx-id data tx-result)]
        (get-anchored-val
          (:db-after tx-result)
          {:entity entity-ident
           :filter {:db/id dbid}})))))

(defn remove-anchor
  [conn user-id anchored-data]
  (when (:sys/entity (d/entity (d/db conn) (:db/id anchored-data)))
    @(d/transact
       conn
       [[:app/retract-entity (:db/id anchored-data) user-id]])
    nil))

