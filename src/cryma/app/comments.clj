(ns cryma.app.comments
  (:require [datomic.api :as d]
            [clojure.core.async :refer [<! <!! >! >!! put! chan go go-loop]]
            [cryma.app.db :as db]
            [taoensso.timbre :refer (log trace debug info warn error)]
            [cryma.core.tools :as tools]))

(defn get-comments-plain [db request & [with-info?]]
  (db/fetch-data
    db
    (-> request
        (update
          :pattern
          #(or %
               [:db/id
                :sys/date
                :comment/body
                {:sys/author [:db/id :user/nickname]}
                {:sys/entity [:db/id :db/ident]}
                {:sys/anchor [:db/id :post/title {:sys/anchor '...}]}]))
        (update
          :order
          #(or %
               [:sys/date :db/id]))
        (update
          :search-fields
          #(or %
               [:comment/body]))
        (assoc :entity :entity.app/comment))
    with-info?))

(defn get-comments [db request & [with-info?]]
  (db/fetch-data
    db
    (-> request
        (update
          :pattern
          #(or %
               [:*
                {:sys/author [:db/id :user/nickname]}
                {:sys/entity [:db/id :db/ident]}
                {:sys/_anchor
                 [:*
                  {:sys/author [:db/id :user/nickname]}
                  {:sys/entity [:db/id :db/ident]}
                  {:sys/_anchor '...}]}]))
        (assoc :entity :entity.app/comment))
    with-info?))

(defn get-comment [db request]
  (first (get-comments db request)))

(defn write-comment
  [conn user-id comment]
  (let [comment (db/prepare-entity-data
                  (d/db conn) comment :entity.app/comment user-id)
        tx-result @(d/transact conn
                               [[:app/add-entity comment user-id]])
        dbid (db/get-tx-id comment tx-result)]
    (get-comment
      (:db-after tx-result)
      {:filter {:db/id dbid}})))

(defn edit-comment
  [conn user-id comment]
  (let [comment (-> comment
                    (select-keys [:db/id :comment/body])
                    tools/prepare-value)
        tx-result @(d/transact conn [[:app/update-entity comment user-id]])
        dbid (db/get-tx-id comment tx-result)]
    (get-comment
      (:db-after tx-result)
      {:filter {:db/id dbid}})))
