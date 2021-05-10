(ns cryma.app.db
  (:require [datomic.api :as d]
            [clojure.zip :as z]
            [cryma.core.tools :as tools])
  (:import (java.util Date)
           (datomic Datom)))

(defn user-access-level [db user-id]
  (let [user-entity (d/entity db user-id)
        ual (when-let [{:keys [:user/access-expiry
                               :sys/archived
                               :user/access-level]} user-entity]
              (when (and (or (nil? access-expiry)
                             (>= (.getTime access-expiry)
                                 (.getTime (Date.))))
                         (not archived)
                         access-level)
                access-level))]
    (if ual
      ual
      (:user/access-level
        (if (and user-id (not= (:db/ident user-entity) :sys.users/guest))
          (d/entity db :sys.users/guest-authenticated)
          (d/entity db :sys.users/guest))))))

(defn get-db-id [plaindb v]
  (if (keyword? v)
    (d/entid plaindb v)
    (:db/id v)))

(defn exclude-attr-filter-ctor [attr db]
  (let [attr-id (get-db-id db attr)]
    (fn [_ ^Datom datom] (not= attr-id (.a datom)))))

(defn exclude-attrs-filter-ctor [attrs db]
  (let [attr-ids (set (map (partial get-db-id db) attrs))]
    (fn [_ ^Datom datom] (nil? (attr-ids (.a datom))))))

(defn new-sys-anchor-filter-ctor [entorents & [author-id]]
  (fn [_]
    (let [entities (set (if (coll? entorents) entorents [entorents]))]
      (if author-id
        (fn [plaindb ^Datom datom]
          (let [entity (d/entity plaindb (.e datom))
                entity-author-id (get-db-id plaindb (:sys/author entity))]
            (or (nil? (entities (:sys/entity entity)))
                (= author-id entity-author-id))))
        (fn [plaindb ^Datom datom]
          (let [entity (d/entity plaindb (.e datom))]
            (nil? (entities (:sys/entity entity)))))))))

(defn anchor-path [db id & [pattern]]
  (let [z (->>
            id
            (d/pull db (or pattern [:db/id {:sys/anchor [:db/id {:sys/anchor '...}]}]))
            (z/zipper :sys/anchor (comp vector :sys/anchor) nil))]
    (loop [z z result []]
      (if (or (z/end? z) (nil? z))
        (->> result reverse vec)
        (recur (z/next z) (conj result (if pattern (z/node z) (:db/id (z/node z)))))))))

(defn get-tx-id [tx-data tx-result]
  (let [dbid (:db/id tx-data)]
    (if (number? dbid)
      dbid
      (d/resolve-tempid
        (:db-after tx-result)
        (:tempids tx-result) dbid))))

(defn vector->map [v]
  (->>
    v
    (partition 2)
    (map vec)
    (into {})))

;;====================================================================
;; DB QUERY
;;====================================================================

(defn make-field-sym [field & [prefix]]
  (let [ns (namespace field)
        n (name field)]
    (symbol (str "?"
                 prefix (when prefix "-")
                 ns (when (seq ns) "-")
                 n))))

(defn -string-contains [src test]
  (.contains (.toLowerCase src)
             (.toLowerCase test)))

(defn string-match [db search s-or-entity & [k]]
  (cond
    (string? s-or-entity)
    (-string-contains s-or-entity search)
    (and k (number? s-or-entity))
    (-string-contains (k (d/pull db [k] s-or-entity) "") search)
    (number? s-or-entity)
    (-string-contains (str s-or-entity) search)
    (keyword? s-or-entity)
    (-string-contains (name s-or-entity) search)
    (set? s-or-entity)
    (some #(-string-contains (k (d/pull db [k] %) "") search) s-or-entity)
    :else
    false))

(defn maybe
  [db e attr else]
  (let [result (d/datoms db :eavt e attr)]
    (if (seq result)
      (if (seq (rest result))
        (into #{} (map :v result))
        (:v (first result)))
      else)))

(defn fts-fields->syms [search-fields]
  (map #(if (= :db/id %) '?e (make-field-sym (if (vector? %) (first %) %) "srch"))
       search-fields))

(defn fts-fields->rule
  [search-fields]
  (if
    (and (vector? search-fields) (seq search-fields))
    (let [fts (vec (concat
                     ['fts '?search]
                     (fts-fields->syms search-fields)))]
      (->>
        search-fields
        (mapv
          (fn [sf]
            (if (= :db/id sf)
              [fts
               [(list 'cryma.app.db/string-match
                      '$ '?search '?e nil)]]
              (let [f (if (vector? sf) (first sf) sf)
                    k (when (vector? sf) (last sf))]
                [fts
                 [(list 'cryma.app.db/string-match
                        '$ '?search (make-field-sym f "srch") k)]]))))))
    '[]))

(defn parse-filter [root-sym filter]
  (->>
    filter
    (mapv
      (fn [[field v]]
        (if (= field :db/id)
          {:binding (if (coll? v) [root-sym '...] root-sym)
           :input   v}
          (let [vr (make-field-sym field)
                binding (if (and (not (map? v)) (coll? v)) [vr '...] vr)]
            {:binding binding
             ;; TODO replace this shitty thing with NOT Just write normal query lang
             :where   (if (and (map? v) (= :not (:op v)))
                        (list 'not [root-sym field vr])
                        [root-sym field vr])
             :input   (if (map? v) (:v v) v)}))))))

(defn parse-pattern [pattern]
  (let [all? (some #{:*} pattern)
        v-pred #(or (when (#{:sys/version} %) [:*])
                    (when
                      (and (map? %)
                           (#{:sys/version} (-> % first key)))
                      (-> % first val)))
        version? (or (some v-pred pattern)
                     (when all? [:*]))]
    {:base    (->> pattern
                   (remove v-pred)
                   vec)
     :version (when version? version?)}))

(defn fetch-data
  [db req & [with-info?]]
  (assert (and (:entity req)
               (or (not (coll? (:entity req)))
                   (seq (:entity req)))) "Entity required to fetch data")
  (let [{:keys [search-string search-fields filter
                pattern entity limit offset] :as req}
        (-> req
            (update :limit #(max (or % 100) 1))
            (update :offset #(max (or % 0) 0)))

        entity (if (coll? entity)
                 (if (= 1 (count entity)) (first entity) (vec entity))
                 entity)
        has-search? (seq search-string)
        ss (when has-search? search-string)
        pattern-info (parse-pattern
                       (or pattern [:* {:sys/entity [:db/id :db/ident]}]))
        filter-info (parse-filter '?e filter)
        need-version? (:version pattern-info)
        query (vec
                (concat
                  '[:find (pull ?e pb)]
                  (when need-version? '[(pull ?tx pv) ?tx-origin])
                  (vec (concat [:in '$ '% (if (coll? entity) '[?entity ...] '?entity) 'pb 'pv '?search]
                               (map :binding filter-info)))
                  (concat
                    [:where
                     ['?e :sys/entity '?entity (if need-version? '?tx-origin '_)]]

                    (when need-version?
                      ['[(datomic.api/q '[:find (max ?tx)
                                          :in $ ?e :where [?e _ _ ?tx]]
                                        $ ?e) [[?tx]]]])
                    (->> filter-info
                         (map :where)
                         (remove nil?)
                         vec)
                    (map
                      (fn [field]
                        (let [field (if (vector? field)
                                      (first field) field)]
                          [(list 'cryma.app.db/maybe
                                 '$ '?e field :nil)
                           (make-field-sym field "srch")]))
                      search-fields))
                  (when (and has-search? (seq search-fields))
                    (list (concat
                            ['fts '?search]
                            (fts-fields->syms search-fields))))))
        result (->>
                 (apply
                   d/q
                   query
                   db
                   (fts-fields->rule search-fields)
                   entity
                   (:base pattern-info) (:version pattern-info)
                   (or ss "")
                   (map :input filter-info))
                 (map (fn [[data version tx-origin]]
                        (if need-version?
                          (assoc data
                            :sys/version
                            (assoc version :tx/origin-id tx-origin))
                          data))))
        ready (->> result
                   (tools/apply-order req)
                   (tools/apply-limit req)
                   vec)]
    (if with-info?
      {:total                           (count result)
       :limit                           limit
       :offset                          offset
       (:value-field with-info? :items) ready}
      ready)))

(defn prepare-entity-data [db entity-data entity-ident & [user-id]]
  (let [{partition   :sys/db.part
         req-date?   :entity/requires-date?
         req-author? :entity/requires-author?}
        (d/pull db [:*
                    {:sys/db.part [:db/ident]}]
                entity-ident)
        part-id (or (:db/ident partition) :db.part/app)]
    (-> entity-data
        tools/prepare-value
        (assoc
          :db/id (d/tempid part-id)
          :sys/entity entity-ident)
        (tools/->if req-date? assoc :sys/date (Date.))
        (tools/->if req-author? assoc :sys/author user-id))))

(defn annotate-tx [tx-data user-id]
  [tx-data
   {:db/id      #db/id[:db.part/tx]
    :sys/author user-id}])

;;====================================================================
;; Generic Schema
;;====================================================================

(def search-schema
  [[:data/type :data-type/map]
   [:map/spec {:search-string
                        [:data/type :data-type/string]
               :pattern [:data/type :data-type/vector]
               :limit   [:data/type :data-type/number]
               :offset  [:data/type :data-type/number]
               :filter  [:data/type :data-type/map]}]])

(defn require-schema [schema]
  (vec (cons [:data/requirement :data-requirement/required] schema)))

(def id-schema
  [:data/type :data-type/long])

(def id-or-ident-schema
  [:data/alternative
   [[:data/type :data-type/long]]
   [[:data/type :data-type/keyword]]])

(def id-required-schema
  [[:data/requirement :data-requirement/required]
   id-schema])

(def non-empty-string-shema
  [[:data/type :data-type/string]
   [:string/not-empty]])

(def non-empty-string-required-shema
  (require-schema non-empty-string-shema))

(def read-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:filter
     [:map/spec-closed
      {:db/id id-required-schema}]}]])