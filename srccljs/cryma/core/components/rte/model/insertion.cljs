(ns cryma.core.components.rte.model.insertion
  (:require
    [clojure.zip :as z]
    [cryma.core.components.rte.model.selection :as s]
    [cryma.core.components.rte.model.tools :as t]
    [cryma.core.components.rte.model.configuration :as conf]
    [cryma.core.components.rte.model.formats :as f]
    [cryma.core.components.rte.model.line-reduce :as lr]
    [cryma.core.components.rte.model.semantics.protocols :as protocols]))

(defmulti do-insert (fn [_ _ ev] (-> ev :item :type)))

(defn collect-materials [z]
  (loop [z z res []]
    (cond (z/end? z)
          res
          (t/zip-is-line? z)
          (recur (z/next z)
                 (if (seq res)
                   (conj res (assoc (t/create-empty-text-material) :content " "))
                   res))
          (t/zip-is-material? z)
          (recur (z/next z) (conj res (z/node z)))
          :else
          (recur (z/next z) res))))

(defn insert-materials [z item wrap-selection? wrap-materials]
  (let [conf (conf/zip-get-conf z)
        materials
        (->> (if (seq wrap-materials) wrap-materials [(t/create-empty-text-material)])
             (reduce (fn [res lat]
                       (let [original-semantics (get-in lat [:meta :original-semantics])
                             lat (if (and original-semantics
                                          (= (:semantics lat) (:semantics item)))
                                   (->
                                     lat
                                     (assoc :semantics original-semantics)
                                     (update :meta dissoc :original-semantics))
                                   lat)]
                         (if-let [lst (peek res)]
                           (if (= (:semantics lst) (:semantics lat))
                             (let [new-format (f/intersect-formats (:format lst) (:format lat))
                                   logic (conf/get-semantics conf (:semantics lst) :logic)
                                   joined (protocols/join-nodes logic lst lat conf)]
                               (t/concatv
                                 (pop res)
                                 (if (rest joined)
                                   joined
                                   [(assoc (first joined) :format new-format)])))
                             (conj res lat))
                           (conj res lat)))) [])
             (map (fn [mat]
                    (let [semantics (:semantics mat)
                          logic (conf/get-semantics conf semantics :logic)
                          deny-wrap? (protocols/deny-wrap? logic mat conf)]
                      (if deny-wrap?
                        mat
                        (merge-with
                          (fn merge-mat [res lat]
                            (if (and (map? res) (map? lat))
                              (merge-with merge-mat res lat)
                              lat))
                          (if wrap-selection?
                            (assoc-in mat [:meta :original-semantics] semantics)
                            mat)
                          (if (and wrap-selection? (empty? (:content item)))
                            (dissoc item :content)
                            item)))))))]
    (loop [z (z/replace z (first materials)) materials (rest materials)]
      (if-let [mat (first materials)]
        (recur (->
                 z
                 (z/insert-right mat)
                 (z/right)) (rest materials))
        z))))

(defmethod do-insert :material
  [z {:keys [start] :as selection} {:keys [item wrap-selection?]}]
  (let [wrap-materials (when wrap-selection?
                         (-> z
                             (s/-copy-selection selection)
                             collect-materials))
        z (if (s/selection-collapsed? selection)
            z (s/zip-delete-selection z selection))
        z (t/zip-get-in (t/doc-zipper (z/root z)) start)
        mat-before (s/zip-delete-selection z {:start start})
        mat-after (s/zip-delete-selection z {:end start})
        z (if (t/-empty? mat-before) z (z/insert-left z (z/node mat-before)))
        z (insert-materials z item wrap-selection? wrap-materials)
        z (if (t/-empty? mat-after) z (z/insert-right z (z/node mat-after)))]
    (t/zip-get-in (t/doc-zipper (z/root z)) (t/zip-end-path z))))

(defn collect-lines [z]
  (loop [z z res []]
    (cond (z/end? z)
          (if (empty? res)
            [(t/create-empty-line)]
            res)
          (-> z z/node :type #{:line})
          (recur (z/next z) (conj res (z/node z)))
          :else
          (recur (z/next z) res))))

(defmethod do-insert :block
  [z {:keys [start] :as selection} {:keys [item wrap-selection?]}]
  (let [wrap-lines (when wrap-selection?
                     (-> z
                         (s/-copy-selection selection)
                         (collect-lines)))
        z (if (s/selection-collapsed? selection)
            z (s/zip-delete-selection z selection))
        z (t/zip-get-in (t/doc-zipper (z/root z)) (take 1 start))
        before (s/zip-delete-selection z {:start start})
        after (s/zip-delete-selection z {:end start})
        z (if (t/-empty? before) z (z/insert-left
                                     z (-> before lr/reduce-lines z/node)))
        z (z/replace z (if wrap-lines (assoc item :content wrap-lines) item))
        z (if (t/-empty? after) z (z/insert-right
                                    z (-> after lr/reduce-lines z/node)))]
    (t/zip-get-in (t/doc-zipper (z/root z)) (t/zip-end-path z))))

(defmethod do-insert :default
  [_ _ ev]
  (throw (ex-info "Unsupported item in :doc.actions/insert" ev)))

(defn process-insert
  [{:keys [doc selection]} ev]
  (do-insert (t/doc-zipper doc) selection ev))

(defmulti do-update (fn [_ _ ev] (-> ev :item :type)))

(defmethod do-update :material
  [z {:keys [start]} {:keys [item]}]
  (->
    z
    (t/zip-get-in start)
    (z/edit merge item)))

(defmethod do-update :block
  [z {:keys [start]} {:keys [item]}]
  (->
    z
    (t/zip-get-in start)
    z/up z/up
    (z/edit merge item)))

(defmethod do-update :default
  [_ _ ev]
  (throw (ex-info "Unsupported item in :doc.actions/update" ev)))

(defn process-update
  [{:keys [doc selection]} ev]
  (let [z (do-update (t/doc-zipper doc) selection ev)]
    (t/zip-get-in (t/doc-zipper (z/root z)) (t/zip-end-path z))))