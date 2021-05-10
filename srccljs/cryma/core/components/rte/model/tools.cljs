(ns cryma.core.components.rte.model.tools
  (:require
    [clojure.zip :as z]
    [clojure.string :as clostr]
    [cryma.core.components.rte.model.semantics.protocols :as protocols]
    [cryma.core.components.rte.model.configuration :as configuration]))

(defn doc-zipper [doc]
  (z/zipper
    (fn [node] (#{:line :block} (:type node)))
    :content
    (fn [node children]
      (assoc node :content (vec children)))
    doc))

(defn concatv [coll & cs]
  (vec (apply concat coll cs)))

(defn butlastv [coll]
  (vec (butlast coll)))

(defn inc-path [path child-index]
  (assert path "path cannot be nil")
  (assert path "child-index cannot be nil")
  (conj path child-index))

(defn zip-branch-pos [z]
  (-> z second :l count))

(defn zip-rightmost? [z]
  (= 0 (-> z second :r count)))

(defn zip-leftmost? [z]
  (= 0 (zip-branch-pos z)))

(defn zip-level [z]
  (-> z second :pnodes count))

(defn zip-root? [z]
  (or (z/end? z) (nil? (z 1)) (-> (z 1) :pnodes nil?)))

(defn zip-type [z]
  (-> z z/node :type))

(defn zip-semantics [z]
  (-> z z/node :semantics))

(defn zip-is-type? [z type]
  (when z
    (= type (zip-type z))))

(defn zip-is-material? [z]
  (zip-is-type? z :material))

(defn zip-is-line? [z]
  (zip-is-type? z :line))

(defn zip-count-inner-positions [z]
  (assert z "Can't count-inner-positions of nil")
  (if (zip-is-material? z)
    (let [conf (configuration/zip-get-conf z)
          logic (configuration/get-semantics conf (zip-semantics z) :logic)]
      (protocols/count-positions logic (z/node z) conf))
    (count (z/children z))))

(defn zip-material-last-position [z]
  (zip-count-inner-positions z))

(defn adjust-inner-position [z position]
  (min position (zip-count-inner-positions z)))

(defn set-material-position [item position]
  (with-meta item (assoc (meta item) :position position)))

(defn get-material-position [item]
  (:position (meta item)))

(defn zip-short-path [z]
  (assert z "Nil zipper passed to zpath")
  (loop [z z path []]
    (if-not (zip-root? z)
      (recur (z/up z) (conj path (zip-branch-pos z)))
      (vec (reverse path)))))

(defn zip-full-path [z]
  (assert (zip-is-material? z) "zip-full-path only applicable to materials")
  (let [base-path (zip-short-path z)
        position (get-material-position (z/node z))]
    (if position
      (inc-path base-path position)
      base-path)))

(defn -zip-path-template [dz zip-stepper material-inner-pos-getter & [relative?]]
  (loop [z dz]
    (if-not (z/branch? z)
      (if relative?
        [(material-inner-pos-getter z)]
        (inc-path (zip-short-path z) (material-inner-pos-getter z)))
      (recur (zip-stepper z)))))

(defn zip-start-path [dz & [relative?]]
  (-zip-path-template dz z/down (constantly 0) relative?))

(defn zip-end-path [dz & [relative?]]
  (-zip-path-template
    dz (comp z/rightmost z/down) zip-material-last-position relative?))

(defn zip-set-material-position [z position]
  (z/edit z set-material-position position))

(defn zip-set-material-position-start [z]
  (zip-set-material-position z 0))

(defn zip-set-material-position-end [z]
  (zip-set-material-position
    z (zip-material-last-position z)))

(defn zip-type-dispatch [& args]
  (-> args first z/node :type))

(defn zip-semantic-dispatch [& args]
  (-> args first z/node :semantics))

(defn item-type-dispatch [& args]
  (-> args first :type))

(defn -zip-reduce-right [z pos-count path]
  (loop [z z cnt pos-count]
    (cond
      (and (zip-rightmost? z) (> cnt 0))
      {:z z :path (zip-end-path z :relative!)}
      (> cnt 0)
      (recur (z/right z) (dec cnt))
      :else
      {:z z :path path})))

(defn zip-get-in [dz path]
  (loop [z dz path path]
    (if-let [p (first path)]
      (if (z/branch? z)
        #_(recur (reduce z/right (z/down z) (range p)) (rest path))
        (let [{:keys [z path]} (-zip-reduce-right (z/down z) p (rest path))]
          (recur z path))
        (zip-set-material-position z p))
      z)))

(defn get-rendering-meta [opendoc & [k]]
  (let [rendering-meta (:rendering-meta (meta opendoc))]
    (if k (get rendering-meta k) rendering-meta)))

(defn update-rendering-meta [opendoc f & args]
  (with-meta opendoc (apply update (meta opendoc) :rendering-meta f args)))

(defn clear-rendering-meta [opendoc]
  (with-meta opendoc (or (dissoc (meta opendoc) :rendering-meta) {})))

;;=============================
;; EMPTY?
;;=============================
(defmulti -empty? zip-type-dispatch)

(defmethod -empty? :material
  [z]
  (= 0 (zip-count-inner-positions z)))

(defmethod -empty? :default
  [z]
  (or
    (nil? (z/node z))
    (empty? (z/children z))
    (loop [z (z/down z)]
      (if (zip-rightmost? z)
        (-empty? z)
        (let [right (z/right z)]
          (if-not (-empty? right)
            false (recur right)))))))

(defn significant-params [item]
  (select-keys item [:type :semantics :format :meta :data]))

(defn equal-params? [params1 params2]
  (every? (fn [k]
            (let [v1 (k params1)
                  v2 (k params2)]
              (or (= v1 v2)
                  (and (nil? v2) (coll? v1) (empty? v1))
                  (and (nil? v1) (coll? v2) (empty? v2))
                  (and (coll? v1) (empty? v1)
                       (coll? v2) (empty? v2)))))
          (set (concat (keys params1) (keys params2)))))

(defn same-params? [node & nodes]
  (let [check (significant-params node)]
    (every? (fn [i] (equal-params? check (significant-params i))) nodes)))

(defn create-empty-text-material [& [template]]
  (merge template
         {:type      :material
          :semantics :text
          :content   ""}))

(defn create-empty-line []
  {:type      :line
   :semantics :generic-line
   :content   [(create-empty-text-material)]})

(defn create-empty-document []
  {:type      :block
   :semantics :document
   :content   [(create-empty-line)]})

;;=============================
;; KEY-ENCODE
;;=============================

(defn doc-path->key [doc path]
  (let [node (get-in doc (interleave (repeat :content) path))
        path (if (and node (not (string? node))) path (pop path))]
    (clostr/join ":" path)))

(defn key->doc-path [key]
  (->>
    (clostr/split key #":")
    (mapv int)))