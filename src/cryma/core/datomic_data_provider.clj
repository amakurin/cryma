(ns cryma.core.datomic-data-provider
  (:require [cryma.core.api :as api]
            [datomic.api :as d])
  (:import (datomic Datom)))

(def schema-sources-key :datomic-data-provider/schema-sources)
(def uri-key :datomic-data-provider/uri)
(def create-onstart-key :datomic-data-provider/create-onstart?)
(def connection-key :datomic-data-provider/connection)
(def default-filter-ctors-key :datomic-data-provider/default-filter-ctors)

(defn create-db [uri schema-source-readers]
  (d/delete-database uri)
  (d/create-database uri)
  (let [conn (d/connect uri)]
    (doseq [reader schema-source-readers]
      (let [edn (api/read-resource reader)]
        (doseq [schema (if (vector? (first edn)) edn [edn])]
          @(d/transact conn schema))))))

(defn build-filtered-db [db filter-ctors]
  (case (count filter-ctors)
    0 db
    1 (d/filter db ((first filter-ctors) db))
    2 (d/filter db
                (let [a ((first filter-ctors) db)
                      b ((second filter-ctors) db)]
                  (fn [plain-db ^Datom datom]
                    (and (a plain-db datom) (b plain-db datom)))))
    3 (d/filter db
                (let [a ((first filter-ctors) db)
                      b ((second filter-ctors) db)
                      c ((second (rest filter-ctors)) db)]
                  (fn [plain-db ^Datom datom]
                    (and (a plain-db datom) (b plain-db datom) (c plain-db datom)))))
    (let [filter-fns (map (fn [ctor] (ctor db)) filter-ctors)]
      (d/filter
        db (fn [plain-db ^Datom datom]
             (every? (fn [ff] (ff plain-db datom)) filter-fns))))))

(defrecord DatomicDataProvider []
  api/IDataProvider
  (get-db [provider]
    (d/db (api/get-conn provider)))
  (get-db-filtered [provider]
    (api/get-db-filtered provider nil))
  (get-db-filtered [provider filter-ctors]
    (let [db (api/get-db provider)
          filter-ctors (vec (concat
                              (default-filter-ctors-key provider)
                              (if (fn? filter-ctors) [filter-ctors] filter-ctors)))]
      (build-filtered-db db filter-ctors)))
  (get-conn [provider]
    (connection-key provider))

  api/IComponent
  (start [provider]
    (let [uri (api/get-conf provider uri-key)]
      (when (create-onstart-key provider)
        (create-db uri (schema-sources-key provider)))
      (assoc provider connection-key (d/connect uri))))
  (stop [provider]
    #_(when (create-onstart-key provider)
      (d/delete-database (api/get-conf provider uri-key)))
    (when-let [conn (connection-key provider)]
      (d/release conn))
    (dissoc provider connection-key)))

(defn new-data-provider [& [m]]
  (map->DatomicDataProvider
    (merge
      {api/id-key               :data-provider
       schema-sources-key       []
       api/configuration-key    {uri-key "datomic:mem://dev"}
       default-filter-ctors-key []}
      m)))