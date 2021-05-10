(ns cryma.core.components.rte.model.typing-in
  (:require
    [clojure.zip :as z]
    [cryma.core.components.rte.model.selection :as s]
    [cryma.core.components.rte.model.tools :as t]
    [cryma.core.components.rte.model.line-reduce :as lr]
    [cryma.core.components.rte.model.configuration :as conf]
    [cryma.core.components.rte.model.semantics.protocols :as protocols]))

(defn typing-zipper [{:keys [doc selection]}]
  (let [dz (t/doc-zipper doc)]
    (if (s/selection-collapsed? selection)
      (t/zip-get-in dz (:start selection))
      (-> dz
          (s/zip-delete-selection selection)
          (t/zip-get-in (:start selection))
          lr/reduce-parent-line))))

;;=============================
;; TYPE-IN
;;=============================

(defmulti -process-type-in t/zip-type-dispatch)

(defn can-type-in-material? [z ev]
  (let [conf (conf/zip-get-conf z)
        logic (conf/get-semantics conf (t/zip-semantics z) :logic)]
    (not (protocols/deny-type-in? logic (z/node z) (peek (t/zip-full-path z)) (:key ev) conf))))

(defn zip-closest-material-left [z]
  (s/zip-closest-material-template
    z {:stepper            z/left
       :neighbor-fn        t/zip-set-material-position-end
       :self-fn            t/zip-set-material-position-start
       :predicate-fn       (constantly true)
       :branch-boundary-fn t/zip-leftmost?}))

(defn zip-closest-material-right [z]
  (s/zip-closest-material-template
    z {:stepper            z/right
       :neighbor-fn        t/zip-set-material-position-start
       :self-fn            t/zip-set-material-position-end
       :predicate-fn       (constantly true)
       :branch-boundary-fn t/zip-rightmost?}))

(defn get-alowed-typein-material [z ev]
  (if (can-type-in-material? z ev)
    z
    (let [right-side? (> (peek (t/zip-full-path z)) 0)
          neighbor-z (if right-side?
                       (zip-closest-material-right z)
                       (zip-closest-material-left z))]
      (if (and neighbor-z (can-type-in-material? neighbor-z ev))
        neighbor-z
        (let [new (t/create-empty-text-material)]
          (t/zip-set-material-position-start
            (if right-side?
              (-> z
                  (z/insert-right new)
                  z/right)
              (-> z
                  (z/insert-left new)
                  z/left))))))))

(defn split-material [z template]
  (let [full-path (t/zip-full-path z)
        last-part (s/zip-delete-selection z {:end full-path})]
    (->
      z
      (s/zip-delete-selection {:start full-path})
      (z/insert-right
        (t/set-material-position (t/create-empty-text-material template) 0))
      z/right
      (#(if (t/-empty? last-part)
         %
         (z/insert-right % (z/node last-part)))))))

(defmethod -process-type-in :material
  [z _ {:keys [format] :as ev}]
  (let [z (get-alowed-typein-material z ev)
        z (if (or (t/-empty? z) (nil? format)
                  (= (:format (z/node z)) format))
            z (split-material z {:format format}))]
    (let [conf (conf/zip-get-conf z)
          logic (conf/get-semantics conf (t/zip-semantics z) :logic)
          {:keys [item position]}
          (protocols/type-in logic (z/node z) (peek (t/zip-full-path z)) (:key ev) conf)]
      (z/replace z (->
                     item
                     (#(if format (assoc % :format format) %))
                     (t/set-material-position position))))))

(defmethod -process-type-in :default
  [z range ev]
  (throw (ex-info "oh god! Cant type in with this item"
                  {:item (z/node z) :selection range :ev ev})))

;;=============================
;; NEW-LINE
;;=============================

(defmulti -process-new-line t/zip-type-dispatch)

(defmethod -process-new-line :material
  [z range ev]
  (-process-new-line (z/up z) range ev))


(defmethod -process-new-line :line
  [z {:keys [start] :as range} ev]
  (let [conf (conf/zip-get-conf z)
        block-z (z/up z)]
    (if-not (protocols/allow-new-line?
              (conf/get-semantics conf (t/zip-semantics block-z) :logic)
              block-z z conf)
      (-process-new-line block-z range ev)
      (-> z
          (s/zip-delete-selection {:start start})
          (#(if (t/-empty? %) (z/replace % (t/create-empty-line)) %))
          (z/insert-right
            (let [line (z/node (s/zip-delete-selection z {:end start}))]
              (if (empty? (:content line))
                (update line :content conj (t/create-empty-text-material))
                line)))
          z/right
          (#(t/zip-get-in (t/doc-zipper (z/root %)) (t/zip-start-path %)))))))

(defmethod -process-new-line :block
  [z {:keys [start]} _]
  (let [deepest (t/zip-get-in (t/doc-zipper (z/root z)) start)]
    (->
      (if (t/-empty? deepest)
        (s/zip-delete-selection z
                                {:start (s/zip-closest-material-prev-path deepest)})
        z)
      (z/insert-right (t/create-empty-line))
      z/right
      ((fn [znew]
         (let [block-frament
               (s/zip-delete-selection z
                                       {:end (s/zip-closest-material-next-path deepest)})]
           (if (t/-empty? block-frament)
             znew (z/insert-right znew (z/node block-frament))))))
      (#(t/zip-get-in (t/doc-zipper (z/root %)) (t/zip-start-path %))))))

;;=============================
;; BACKSPACE
;;=============================

(defmulti -process-backspace t/zip-type-dispatch)

(defn -backspace-del-template
  [z range del-range-fn del-range-key]
  (let [full-path (t/zip-full-path z)
        position (last full-path)
        del-path (del-range-fn z position)
        del-path (cond
                   (number? del-path)
                   (conj (t/butlastv full-path) del-path)
                   (vector? del-path) del-path
                   :else
                   (throw (ex-info "del-range-fn should return vector or position"
                                   {:del-path  del-path
                                    :full-path full-path})))]
    (if (= del-path (del-range-key range))
      z
      (-> (t/doc-zipper (z/root z))
          (s/zip-delete-selection (assoc range del-range-key del-path))
          (t/zip-get-in (if (= :start del-range-key) del-path (:start range)))
          lr/reduce-parent-line))))

(defn child-path? [parent chk]
  (let [cparent (count parent)
        cchk (count chk)]
    (if (> cparent cchk)
      false
      (= (subvec chk 0 cparent) parent))))

(defn same-line-path? [z check-path]
  (child-path? (t/zip-short-path z) check-path))

(defn backspace-prev-position [z position]
  (if (> position 0)
    (dec position)
    (let [prev-path (s/zip-closest-material-prev-path z)
          prev-pos (peek prev-path)]
      (if (and (same-line-path? (z/up z) prev-path)
               (> prev-pos 0))
        (conj (pop prev-path) (dec prev-pos))
        prev-path))))

(defmethod -process-backspace :material
  [z range _]
  (-backspace-del-template z range backspace-prev-position :start))

(defmethod -process-backspace :default
  [z range ev]
  (throw (ex-info "oh god! Cant backspace with this item"
                  {:item (z/node z) :selection range :ev ev})))

;;=============================
;; DEL
;;=============================

(defmulti -process-del t/zip-type-dispatch)

(defn del-next-position [z position]
  (if (< position (t/zip-count-inner-positions z))
    (inc position)
    (let [next-path (s/zip-closest-material-next-path z)
          next-pos (peek next-path)]
      (if (and (same-line-path? (z/up z) next-path)
               (< next-pos
                  (-> (t/doc-zipper (z/root z))
                      (t/zip-get-in next-path)
                      (t/zip-count-inner-positions))))
        (conj (pop next-path) (inc next-pos))
        next-path))))

(defmethod -process-del :material
  [z range _]
  (-backspace-del-template z range del-next-position :end))

(defmethod -process-del :default
  [z range ev]
  (throw (ex-info "oh god! Cant del with this item"
                  {:item (z/node z) :selection range :ev ev})))

