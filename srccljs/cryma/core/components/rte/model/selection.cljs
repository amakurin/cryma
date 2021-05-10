(ns cryma.core.components.rte.model.selection
  (:require
    [clojure.zip :as z]
    [cryma.core.components.rte.model.tools :as t]
    [cryma.core.components.rte.model.semantics.protocols :as protocols]
    [cryma.core.components.rte.model.configuration :as configuration]))

;;=============================
;; SELECTION
;;=============================
(defn zip-closest-material-template
  [z {:keys [stepper neighbor-fn self-fn
             predicate-fn branch-boundary-fn]}]
  (loop [zcur z]
    (let [stepped-z (stepper zcur)]
      (cond
        (or (nil? stepped-z) (z/end? stepped-z))
        (self-fn z)
        (t/zip-is-material? stepped-z)
        (if (and (not (predicate-fn stepped-z))
                 (not (branch-boundary-fn stepped-z)))
          (recur stepped-z)
          (neighbor-fn stepped-z))
        :else
        (recur stepped-z)))))

(defn zip-closest-material-prev [z]
  (zip-closest-material-template
    z {:stepper            z/prev
       :neighbor-fn        t/zip-set-material-position-end
       :self-fn            t/zip-set-material-position-start
       :predicate-fn       (comp not t/-empty?)
       :branch-boundary-fn t/zip-leftmost?}))

(defn zip-closest-material-next [z]
  (zip-closest-material-template
    z {:stepper            z/next
       :neighbor-fn        t/zip-set-material-position-start
       :self-fn            t/zip-set-material-position-end
       :predicate-fn       (comp not t/-empty?)
       :branch-boundary-fn t/zip-rightmost?}))

(defn zip-closest-material-prev-path [z]
  (t/zip-full-path (zip-closest-material-prev z)))

(defn zip-closest-material-next-path [z]
  (t/zip-full-path (zip-closest-material-next z)))

(defn normalize-path [doc base-path & [index?]]
  (let [z (t/zip-get-in (t/doc-zipper doc) base-path)
        material? (t/zip-is-material? z)]
    (cond
      (and material? index?)
      (t/inc-path base-path (t/adjust-inner-position z index?))
      (and material? (= (t/zip-full-path z) base-path))
      base-path
      material?
      (t/zip-start-path z)
      (and index? (= 0 index?))
      (t/zip-start-path (t/zip-get-in (t/doc-zipper doc) (conj base-path index?)))
      index?
      (t/zip-end-path (t/zip-get-in (t/doc-zipper doc) (conj base-path (dec index?))))
      :else
      (t/zip-start-path z))))

(defn selection-collapsed? [selection]
  (= (:start selection) (:end selection)))

(defn collapsed-selection [path]
  {:start path :end path})

(defn init-selection-start [doc]
  (let [start (t/zip-start-path (t/doc-zipper doc))]
    (collapsed-selection start)))

(defn init-selection-end [doc]
  (let [end (t/zip-end-path (t/doc-zipper doc))]
    (collapsed-selection end)))

(defn zip-collapsed-selection [z]
  (collapsed-selection (t/zip-full-path z)))

;;=============================
;; JOIN ITEMS
;;=============================
(defmulti -join-nodes
          (fn [start-item end-item _] [(:type start-item) (:type end-item)]))

(defn join-at-pos [pos nodes conf]
  (loop [res [] nodes nodes i 0]
    (if-let [current-node (first nodes)]
      (let [new-res
            (if (= i (inc pos))
              (let [last-node (peek res)
                    joined-result (-join-nodes last-node current-node conf)]
                (vec (concat
                       (butlast res)
                       joined-result)))
              (conj res current-node))]
        (recur new-res (rest nodes) (inc i)))
      res)))

;;-----------------------------
;; TYPEs DISPATCH
;;-----------------------------
(defmethod -join-nodes [:material :material]
  [start-node end-node conf]
  (let [start-logic (configuration/get-semantics conf (:semantics start-node) :logic)
        end-logic (configuration/get-semantics conf (:semantics end-node) :logic)
        start-empty? (= 0 (protocols/count-positions start-logic start-node conf))
        end-empty? (= 0 (protocols/count-positions end-logic end-node conf))]
    (cond
      (and start-empty? (not end-empty?))
      [end-node]
      end-empty?
      [start-node]
      (t/same-params? start-node end-node)
      (protocols/join-nodes start-logic start-node end-node conf)
      :else
      [start-node end-node])))

(defn join-inner-nodes [start-nodes to-join concat-tail? conf]
  (let [start-last-node (last start-nodes)
        end-nodes to-join
        end-first-node (first end-nodes)]
    (t/concatv
      (butlast start-nodes)
      (cond (and start-last-node end-first-node)
            (-join-nodes start-last-node end-first-node conf)
            start-last-node [start-last-node]
            end-first-node [end-first-node])
      (when concat-tail? (rest end-nodes)))))

(defmethod -join-nodes [:line :line]
  [start-node end-node conf]
  (if (t/same-params? start-node end-node)
    [(update start-node :content
             join-inner-nodes (:content end-node) :concat-tail! conf)]
    [start-node end-node]))

(defmethod -join-nodes [:line :block]
  [start-node end-node conf]
  (let [start-line start-node
        block-nodes (:content end-node)
        block-first-node (first block-nodes)
        joined-nodes (-join-nodes start-line block-first-node conf)
        inner-block (when (-> joined-nodes last :type #{:block})
                      (last joined-nodes))
        joined-nodes (if inner-block (t/butlastv joined-nodes) joined-nodes)]
    (t/concatv
      joined-nodes
      (when-let [tail (seq (concat
                             (when inner-block [inner-block])
                             (rest block-nodes)))]
        [(assoc end-node :content (vec tail))]))))

(defmethod -join-nodes [:block :line]
  [start-node end-node conf]
  [(update start-node :content
           join-inner-nodes [end-node] false conf)])

(defmethod -join-nodes [:block :block]
  [start-node end-node conf]
  (if (t/same-params? start-node end-node)
    [(update start-node :content
             join-inner-nodes (:content end-node) :concat-tail! conf)]
    (->>
      [(update start-node :content
               join-inner-nodes (:content end-node) false conf)
       (when-let [tail (-> end-node :content next)]
         (assoc end-node :content (vec tail)))]
      (remove nil?) vec)))

;;=============================
;; COPY\DELETE SELECTION
;;=============================
(defmulti -copy-selection t/zip-type-dispatch)

(defn zip-copy-selection
  [dz selection & [inverse?]]
  (-copy-selection dz selection inverse?))

(defn zip-copy-selection-minimized [dz selection]
  (let [zfrag (zip-copy-selection dz selection)]
    (loop [z zfrag]
      (let [node (z/node z)]
        (cond
          (= :line (:type node))
          (t/doc-zipper (assoc (z/node zfrag) :content [node]))
          (= 1 (count (z/children z)))
          (recur (z/down z))
          :else zfrag)))))

(defn zip-delete-selection [dz selection]
  (zip-copy-selection dz selection :inverse!))

(defn path-relative-location [base-path check-path]
  (loop [base-path base-path check-path check-path]
    (let [b (first base-path)
          c (first check-path)]
      (cond
        (nil? c)
        :equal
        (nil? b)
        :inner
        (= b c)
        (recur (rest base-path) (rest check-path))
        (< c b)
        :before
        (> c b)
        :after))))

(defn adjust-child-path [base-path child-path z boundary inverse?]
  (let [relative-location (path-relative-location base-path child-path)]
    (cond
      (= :inner relative-location) child-path
      (and (= :after relative-location) (= :start boundary) inverse?)
      (t/zip-end-path z)
      (and (= :before relative-location) (= :end boundary) inverse?)
      (t/zip-start-path z))))

(defn child-path->pos [base-path child-path]
  (first (drop (count base-path) child-path)))

(defn prepare-selection [z {:keys [start end]} inverse?]
  (let [base-path (t/zip-short-path z)
        start (adjust-child-path base-path start z :start inverse?)
        end (adjust-child-path base-path end z :end inverse?)]
    {:start     start
     :end       end
     :pos-start (child-path->pos base-path start)
     :pos-end   (child-path->pos base-path end)}))

(defmethod -copy-selection :material
  [z selection & [inverse?]]
  (let [{:keys [pos-start pos-end]} (prepare-selection z selection inverse?)
        conf (configuration/zip-get-conf z)
        logic (configuration/get-semantics conf (t/zip-semantics z) :logic)]
    (z/edit z #(protocols/copy-selection logic % pos-start pos-end inverse? conf))))

(defn between? [i pos-start pos-end]
  (and
    (or (nil? pos-start) (> i pos-start))
    (or (nil? pos-end) (< i pos-end))))

(defmethod -copy-selection :default
  [z selection & [inverse?]]
  (let [inside-fn? (if inverse? (comp not between?) between?)
        {:keys [start end pos-start pos-end] :as selection}
        (prepare-selection z selection inverse?)
        pos-last (dec (count (z/children z)))]
    (loop [z (z/down z) i 0]
      (let [z (cond
                (= i pos-start pos-end)
                (-copy-selection z selection inverse?)
                (= i pos-start)
                (-copy-selection z {:start start} inverse?)
                (= i pos-end)
                (-copy-selection z {:end end} inverse?)
                (not (inside-fn? i pos-start pos-end))
                (z/replace z nil)
                :else z)]
        (if (< i pos-last)
          (recur (z/right z) (inc i))
          (z/edit
            (z/up z)
            update :content
            (fn [content]
              (let [content (vec (remove nil? content))]
                (if (and inverse? pos-start pos-end
                         (not= pos-start pos-end))
                  (join-at-pos pos-start content (configuration/zip-get-conf z))
                  content)))))))))

;;=============================
;; SELECTION STATE
;;=============================

(defn material-middle? [z path start end]
  (let [start-path (pop start)
        end-path (pop end)]
    (and (t/zip-is-material? z)
         (or
           (= start end)
           (not= start-path path)
           (not= (t/zip-material-last-position z) (peek start)))
         (or (not= end-path path)
             (not= 0 (peek end))))))

(defn get-basic-formatting [z {:keys [start end]}]
  (let [end-path (pop end)]
    (loop [z (t/zip-get-in z start) semantics #{} format #{}]
      (let [path (t/zip-short-path z)
            material? (t/zip-is-material? z)
            semantics (if material? (conj semantics (-> z z/node :semantics)) semantics)
            format (if (material-middle? z path start end)
                     (conj format (-> z z/node :format))
                     format)]
        (if (or (= path end-path) (z/end? z))
          (let [semantics (when (= 1 (count semantics)) (first semantics))
                format (when (= 1 (count format)) (first format))]
            {:semantics semantics :format format})
          (recur (z/next z) semantics format))))))

(defn get-block-formatting [z {:keys [start end]}]
  (if-let [index (and (= (first start) (first end)) (first start))]
    (let [z (t/zip-get-in z [index])
          node (z/node z)]
      (if (= :block (:type node))
        (select-keys node [:semantics :format])
        {:semantics nil :format nil}))
    {:semantics nil :format nil}))

(defn selection-state [{:keys [doc selection]}]
  (let [z (t/doc-zipper doc)
        block-formating (get-block-formatting z selection)
        current-material-z (when (= (pop (:start selection))
                                    (pop (:end selection)))
                             (t/zip-get-in (t/doc-zipper doc) (:start selection)))
        current-material (when current-material-z (z/node current-material-z))]
    {:basic-formatting (get-basic-formatting z selection)
     :block-formatting block-formating
     :collapsed?       (selection-collapsed? selection)
     :current-material current-material
     :current-block    (when current-material-z
                         (-> current-material-z z/up z/up z/node))
     :multimaterial?   (nil? current-material)
     :multiline?       (not= (->> selection :start (drop-last 2))
                             (->> selection :end (drop-last 2)))
     :multiblock?      (-> block-formating :semantics nil?)}))

(defn update-selection-state [opendoc]
  (assoc opendoc :selection-state (selection-state opendoc)))

(defn update-selection [opendoc selection]
  (->
    opendoc
    (assoc :selection selection)
    update-selection-state))
