(ns cryma.core.components.cursors
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :refer [<!]]
            [rum.core :as rum]))

(defprotocol ICursor
  (-value [x])
  (-path [cursor])
  (-src [cursor]))

(defn cur? [x]
  (satisfies? ICursor x))

(defprotocol ICursorDerive
  (-derive [cursor value path]))

(declare cur)
(declare throw-not-supported)

(extend-type default
  ICursorDerive
  (-derive [this value path]
    (cur value (-src this) path (-meta this))))

(deftype MapCursor [value src path meta]
  Object
  (toString [_]
    (str "#MapCursor [ " (pr-str value) "]"))
  (equiv [this other]
    (-equiv this other))
  IWithMeta
  (-with-meta [_ new-meta]
    (MapCursor. value src path new-meta))
  IMeta
  (-meta [_] meta)
  IDeref
  (-deref [_]
    (get-in @src path))
  ICursor
  (-value [_] value)
  (-path [_] path)
  (-src [_] src)
  ICloneable
  (-clone [_]
    (MapCursor. value src path meta))
  ICounted
  (-count [_]
    (-count value))
  ICollection
  (-conj [this o]
    (throw-not-supported this "Conj" {:conj o}))
  IEmptyableCollection
  (-empty [this]
    (throw-not-supported this "Empty"))
  ILookup
  (-lookup [this k]
    (-derive this (-lookup value k) (conj path k)))
  (-lookup [this k not-found]
    (let [v (-lookup value k ::not-found)]
      (if-not (= v ::not-found)
        (-derive this v (conj path k))
        (if (identical? not-found cljs.core/lookup-sentinel)
          (-derive this nil (conj path k))
          not-found))))
  IFn
  (-invoke [this k]
    (-lookup this k))
  (-invoke [this k not-found]
    (-lookup this k not-found))
  ISeqable
  (-seq [this]
    (when (pos? (-count this))
      (map (fn [[k v]]
             [k (-derive this v (conj path k))])
           value)))
  IAssociative
  (-contains-key? [_ k]
    (-contains-key? value k))
  (-assoc [this k v]
    (throw-not-supported this "Assoc" {:assoc {:k k :v v}}))
  IMap
  (-dissoc [this k]
    (throw-not-supported this "Dissoc" {:dissoc {:k k}}))
  IEquiv
  (-equiv [_ other]
    (if (cur? other)
      (= value (-value other))
      (= value other)))
  IHash
  (-hash [_]
    (hash value))
  IKVReduce
  (-kv-reduce [this _ _]
    (throw-not-supported this "KVReduce"))
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer "#MapCursor [ ")
    (pr-writer {:val    value
                :actual (-deref this)} writer opts)
    (-write writer "]")))

(deftype IndexedCursor [value src path meta]
  Object
  (toString [_]
    (str "#IndexedCursor [ " (pr-str value) "]"))
  (equiv [this other]
    (-equiv this other))
  ISequential
  IDeref
  (-deref [_]
    (get-in @src path))
  IWithMeta
  (-with-meta [_ new-meta]
    (IndexedCursor. value src path new-meta))
  IMeta
  (-meta [_] meta)
  ICursor
  (-value [_] value)
  (-path [_] path)
  (-src [_] src)
  ICloneable
  (-clone [_]
    (IndexedCursor. value src path meta))
  ICounted
  (-count [_]
    (-count value))
  ICollection
  (-conj [this o]
    (throw-not-supported this "Conj" {:conj o}))
  IEmptyableCollection
  (-empty [this]
    (throw-not-supported this "Empty"))
  ILookup
  (-lookup [this n]
    (-nth this n))
  (-lookup [this n not-found]
    (-nth this n not-found))
  IFn
  (-invoke [this k]
    (-lookup this k))
  (-invoke [this k not-found]
    (-lookup this k not-found))
  IVector
  (-assoc-n [this _ _]
    (throw-not-supported this "Assoc-n"))
  IIndexed
  (-nth [this n]
    (-derive this (-nth value n) (conj path n)))
  (-nth [this n not-found]
    (if (< n (-count this))
      (-derive this (-nth value n) (conj path n))
      not-found))
  ISeqable
  (-seq [this]
    (when (pos? (-count this))
      (map-indexed (fn [i v] (-derive this v (conj path i))) value)))
  IAssociative
  (-contains-key? [_ k]
    (-contains-key? value k))
  (-assoc [this n v]
    (throw-not-supported this "Assoc" {:assoc {:k n :v v}}))
  IStack
  (-peek [this]
    (throw-not-supported this "Peek"))
  (-pop [this]
    (throw-not-supported this "Pop"))
  IEquiv
  (-equiv [_ other]
    (if (cur? other)
      (= value (-value other))
      (= value other)))
  IHash
  (-hash [_]
    (hash value))
  IKVReduce
  (-kv-reduce [this _ _]
    (throw-not-supported this "KVReduce"))
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer "#IndexedCursor [ ")
    (pr-writer {:val    value
                :actual (-deref this)} writer opts)
    (-write writer "]")))

(deftype PrimitiveCursor [value src path meta]
  Object
  (toString [_]
    (str "#PrimitiveCursor [ " (pr-str value) "]"))
  (equiv [this other]
    (-equiv this other))
  ICloneable
  (-clone [_] (PrimitiveCursor. value src path meta))
  ISeqable
  (-seq [_] (if (seq? value) value nil))
  IEquiv
  (-equiv [_ other]
    (if (cur? other)
      (= value (-value other))
      (= value other)))
  IDeref
  (-deref [_]
    (get-in @src path))
  IWithMeta
  (-with-meta [_ new-meta]
    (PrimitiveCursor. value src path new-meta))
  IMeta
  (-meta [_] meta)
  ICursor
  (-value [_] value)
  (-path [_] path)
  (-src [_] src)
  IPrintWithWriter
  (-pr-writer [this writer opts]
    (-write writer "#PrimitiveCursor [ ")
    (pr-writer {:val    value
                :actual (-deref this)} writer opts)
    (-write writer "]")))

(defn throw-not-supported [cursor meth-name & [add-info]]
  (throw
    (ex-info
      (str meth-name " is not supported on cursors. Use deref or swap.")
      (merge
        {:cursor {:path  (-path cursor)
                  :state (-src cursor)
                  :type  (cond
                           (instance? MapCursor cursor)
                           :map
                           (instance? IndexedCursor cursor)
                           :indexed
                           (instance? PrimitiveCursor cursor)
                           :primitive
                           :else
                           (type cursor))}}
        add-info))))


(defn new-meta []
  {:sys.cursors/reserved (atom {})})

(defn cur
  ([value]
   (cond
     (cur? value)
     value
     (satisfies? IDeref value)
     (cur @value value [] (new-meta))
     :else
     (cur value (atom value) [] (new-meta))))
  ([value src path meta]
   (cond
     (indexed? value) (IndexedCursor. value src path meta)
     (map? value) (MapCursor. value src path meta)
     :else (PrimitiveCursor. value src path meta))))

(defn cval [cur-or-val]
  (if (cur? cur-or-val) @cur-or-val cur-or-val))

(defn actual [x]
  (assert (cur? x) "cant actualize! non cursor")
  (cur @x (-src x) (-path x) (meta x)))

(defn rst!
  [x new-v]
  (assert (cur? x) "cant rst! non cursor")
  (let [src (-src x)
        path (-path x)]
    (if (seq path)
      (swap! src assoc-in path new-v)
      (reset! src new-v))
    (cur new-v src path (meta x))))

(defn swp!
  [x f & args]
  (assert (cur? x) "cant swp! non cursor")
  (let [src (-src x)
        path (-path x)
        result (if (seq path)
                 (swap! src update-in path
                        (fn [v] (apply f v args)))
                 (apply swap! src f args))]

    (cur (get-in result path)
         src path (meta x))))

;; Reserved for strange ideas
(defn cur-reserved [x]
  (assert (cur? x) "cant cur-reserved non cursor")
  (:sys.cursors/reserved (meta x)))

#_(defn swp-reserved! [x f & args]
    (assert (cur? x) "cant swp-reserved! non cursor")
    (let [reserved (cur-reserved x)]
      (apply swap! reserved f args)))

(defn local-path [k]
  [:sys.cursored/local-cursors k])

(defn non-watchable-path [k]
  [:sys.cursored/non-watchables k])

(defn non-watchable [state k]
  (get-in state (non-watchable-path k)))

(defn local [state k]
  (get-in state (local-path k)))

(defn cursor [state index]
  (let [state (if-let [state-ref (:sys.cursored/state-ref state)] @state-ref state)]
    (get-in state [:rum/args index])))


(defn child-path? [parent chk]
  (let [cparent (count parent)
        cchk (count chk)]
    (if (> cparent cchk)
      false
      (= (subvec chk 0 cparent) parent))))

(defn relative-child-path [full-path abs-path]
  (let [cfull (count full-path)
        cabs (count abs-path)]
    (if (> cfull cabs)
      (subvec full-path cabs cfull)
      [])))

(defn get-watchable-cursors [state]
  (->>
    state :rum/args
    (filter cur?)
    (concat (map val (:sys.cursored/local-cursors state)))
    vec))

(defn- deref-watchable-cursors [state]
  (->> (get-watchable-cursors state)
       (mapv deref)))

(defn prepare-state-val [state]
  (-> state
      (dissoc :sys.cursored/state-ref)
      (update :rum/args vec)))

(defn cursored-props [_ _ _])

(defn- parent-cursors [state]
  (vec
    (aget (aget (:rum/react-component state) "context") "sysParentCursors")))

(defn watched-by-parent? [parent-cursors ref path]
  (some
    (fn [c]
      (and (= ref (aget c "ref"))
           (child-path? (aget c "path") path)))
    parent-cursors))

(defn cursored [& [local-cursors non-watchables]]
  (assert (or (nil? local-cursors) (map? local-cursors))
          "local-cursors should be or nil or map of some-k to cursor-or-curorValue")
  (assert (or (nil? non-watchables) (map? non-watchables))
          "non-watchables should be or nil or map of some-k to cursor-or-curorValue")
  {:will-mount
   (fn [state]
     (let [local-cursors (->>
                           local-cursors
                           (map (fn [[k lc]] [k (if (cur? lc) lc (cur lc))]))
                           (into {}))
           non-watchables (->>
                            non-watchables
                            (map (fn [[k lc]] [k (if (cur? lc) lc (cur lc))]))
                            (into {}))
           state (-> state
                     (assoc :sys.cursored/state-ref (atom {}))
                     (assoc :sys.cursored/local-cursors local-cursors)
                     (assoc :sys.cursored/non-watchables non-watchables))
           id (:rum/id state)
           state-ref (:sys.cursored/state-ref state)
           parent-cursors (parent-cursors state)]
       (reset! state-ref (prepare-state-val state))
       (doseq [c (get-watchable-cursors state)]
         (let [path (-path c)]
           (when-not (watched-by-parent? parent-cursors (-src c) path)
             (add-watch
               (-src c) id
               (fn [_ _ old new]
                 (when (not= (get-in old path)
                             (get-in new path))
                   (rum/request-render (:rum/react-component @state-ref))))))))
       state))
   :should-update
   (fn [old-state new-state]
     (let []
       #_(println "SHOULD::: " (:rum/id new-state)
                  (not= (:sys.cursored/prev-state old-state)
                        (deref-watchable-cursors new-state))
                  "\n -----OLD" #_(:sys.cursored/prev-state old-state)
                  "\n -----NEW" #_(deref-watchable-cursors new-state))
       (not= (:sys.cursored/prev-state old-state)
             (deref-watchable-cursors new-state))))

   :child-context
   (fn [state]
     {:sysParentCursors
      (vec
        (concat
          (parent-cursors state)
          (map (fn [c] #js {:ref (-src c) :path (-path c)})
               (get-watchable-cursors state))))})

   ;; NOTE::snoop: workaround merge at rum.core line 45
   :class-properties
   {:contextTypes
    {:sysChannels      cursored-props
     :sysParentCursors cursored-props}
    :childContextTypes
    {:sysChannels      cursored-props
     :sysParentCursors cursored-props}}

   :will-update
   (fn [state]
     (reset! (:sys.cursored/state-ref state) (prepare-state-val state))
     state)
   :wrap-render
   (fn [render-fn]
     (fn [state]
       (let [[dom next-state] (render-fn state)]
         [dom (assoc next-state :sys.cursored/prev-state
                                (deref-watchable-cursors state))])))
   :transfer-state
   (fn [old new]
     (let [state-ref (:sys.cursored/state-ref old)]
       (assoc new
         :sys.cursored/state-ref state-ref
         :sys.cursored/local-cursors (:sys.cursored/local-cursors old)
         :sys.cursored/non-watchables (:sys.cursored/non-watchables old)
         :sys.cursored/prev-state (:sys.cursored/prev-state old))))
   :will-unmount
   (fn [state]
     (let [id (:rum/id state)]
       (doseq [c (get-watchable-cursors state)]
         (remove-watch (-src c) id)))
     state)})

(defn parent-cursor
  [child-cursor & [distance]]
  (let [src (-src child-cursor)
        path (vec (drop-last (or distance 1) (-path child-cursor)))]
    (cur (get-in @src path) src path (-meta child-cursor))))

(defn- get-cursor* [state state-cursor-path]
  (get-in state state-cursor-path))

(defn target-child? [{:keys [target-child?]}]
  (cond
    (or (nil? target-child?) (vector? target-child?))
    target-child?
    (cur? target-child?)
    (-path target-child?)
    :else
    (#{:all} target-child?)))

(defn cursored-chans [main-cursor-path & [event-handler]]
  {:will-mount
   (fn [state]
     (if-let [{:keys [make-ch4read make-ch4write]}
              (:sys.channels/api state)]
       (let [state-ref (:sys.cursored/state-ref state)
             ch4read
             (make-ch4read
               (comp
                 (filter
                   (fn [ev]
                     (let [c (get-cursor* @state-ref main-cursor-path)
                           path (-path c)
                           dp (:data-path ev)
                           target-child (target-child? ev)]
                       (and
                         (or (= (-src c) (and (:src-cursor ev) (-src (:src-cursor ev))))
                             (:cursors/broadcast? ev))
                         (or (nil? dp)
                             (= path target-child)
                             (and (= :all target-child) (child-path? dp path))
                             (child-path? path dp))))))
                 (map (fn [ev]
                        (let [c (get-cursor* @state-ref main-cursor-path)
                              path (-path c)
                              dp (:data-path ev)]
                          (-> ev
                              (assoc :data-path (relative-child-path dp path))
                              (assoc :tgt-cursor c)))))))
             ch4write
             (make-ch4write
               (map (fn [ev]
                      (if (and (:data-path ev) (:src-cursor ev)
                               (not (:rewrite-data-path ev)))
                        ev
                        (let [c (get-cursor* @state-ref main-cursor-path)
                              path (-path c)]
                          (-> ev
                              (assoc :data-path path)
                              (assoc :src-cursor c)))))))
             state (-> state
                       (assoc :sys.channels/chans
                              {:ch4read ch4read :ch4write ch4write}))]
         (swap! state-ref assoc :sys.channels/chans (:sys.channels/chans state))
         (go
           (loop []
             (when-let [ev (<! ch4read)]
               (when event-handler
                 (event-handler @state-ref ev))
               (recur)))
           #_(debug "read loop exit rum/id" (:rum/id state)))
         state)
       state))

   ;; NOTE::snoop: workaround merge at rum.core line 45
   :class-properties
   {:contextTypes
    {:sysChannels      cursored-props
     :sysParentCursors cursored-props}
    :childContextTypes
    {:sysChannels      cursored-props
     :sysParentCursors cursored-props}}
   })

