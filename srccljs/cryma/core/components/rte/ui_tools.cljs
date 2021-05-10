(ns cryma.core.components.rte.ui-tools
  (:require
    [goog.dom :as gd]
    [cryma.core.components.cursors :as c]
    [cryma.core.components.rte.model.tools :as t]
    [cryma.core.components.rte.model.selection :as s]
    [cryma.core.components.rte.model.configuration :as conf]
    [cryma.core.components.rte.model.semantics.protocols :as protocols]))

(defn get-doc [cursor]
  (:doc @(c/-src cursor)))

(defn doc-cursor->key [cursor]
  (t/doc-path->key
    (get-doc cursor)
    (->>
      (c/-path cursor)
      (filter number?)
      vec)))

(defn cursor->conf [cursor]
  (conf/get-conf (get-doc cursor)))

(defn semantic-conf [node-cursor k & [not-found]]
  (let [semantics (:semantics @node-cursor)]
    (if-let [conf (cursor->conf node-cursor)]
      (if-let [v (conf/get-semantics conf semantics k)]
        v not-found)
      not-found)))

(defn semantic-renderer [item-cursor]
  (semantic-conf item-cursor :component))

(defn get-tag [node-cursor]
  (let [conf (cursor->conf node-cursor)
        logic (semantic-conf node-cursor :logic)]
    (or (when logic (protocols/html-tag logic @node-cursor conf)) :div)))

(defn get-line-tag [line-cursor]
  (let [conf (cursor->conf line-cursor)
        block-cursor (c/parent-cursor line-cursor 2)
        block-logic (semantic-conf block-cursor :logic)]
    (or (when block-logic (protocols/html-line-tag block-logic @block-cursor conf)) :p)))

(defn current-selection-state [opendoc]
  (or
    (:selection-state-override opendoc)
    (:selection-state opendoc)))

(defn current-selection-val [opendoc ks]
  (get-in (current-selection-state opendoc) ks))

(defn dom-node->dom-element [dom-node]
  (if (gd/isElement dom-node)
    dom-node
    (.-parentElement dom-node)))

(defn get-attribute [dom-node k]
  (when-let [el (dom-node->dom-element dom-node)]
    (.getAttribute el (name k))))

(defn doc-path->dom-node [doc root-node path]
  (let [k (t/doc-path->key doc path)
        node (gd/findNode
               root-node
               (fn [n]
                 (and (gd/isElement n)
                      (= k
                         (get-attribute n :data-keks-id)))))
        first-child (.-firstChild node)]
    (if (gd/isElement first-child) node first-child)))

(defn dom-node->doc-path [dom-node]
  (if-let [n (gd/getAncestor
               dom-node
               (fn [n] (and (gd/isElement n) (.getAttribute n "data-keks-id")))
               true)]
    (t/key->doc-path (.getAttribute n "data-keks-id"))
    [0]))

(defn dom-selection->doc-selection-raw [dom-selection]
  (if (and dom-selection (> (.-rangeCount dom-selection) 0))
    (let [rng (.getRangeAt dom-selection 0)
          start-path (dom-node->doc-path (.-startContainer rng))
          start-index (.-startOffset rng)
          end-path (dom-node->doc-path (.-endContainer rng))
          end-index (.-endOffset rng)]
      {:start (conj start-path start-index)
       :end   (conj end-path end-index)})
    {:start [0 0] :end [0 0]}))

(defn dom-selection->doc-selection [doc dom-selection]
  (let [{:keys [start end]}
        (dom-selection->doc-selection-raw dom-selection)]
    {:start (s/normalize-path doc (pop start) (peek start))
     :end   (s/normalize-path doc (pop end) (peek end))}))

(defn acualize-selection [opendoc dom-selection & [reflected?]]
  ;NOTE::snoop: this reflected? check serves to coordinate fast keyboard input with selection rendering
  (if (or (t/get-rendering-meta opendoc :selection-reflected?) reflected?)
    (let [selection (dom-selection->doc-selection (:doc opendoc) dom-selection)]
      (if (not= selection (:selection opendoc))
        (->
          opendoc
          (t/update-rendering-meta assoc :selection-reflected? reflected?)
          (s/update-selection selection)
          (dissoc :selection-state-override))
        opendoc))
    opendoc))

(defn -get-node [state]
  (js/ReactDOM.findDOMNode (:rum/react-component state)))

(defn get-node [state take-parent-by-class?]
  (let [node (-get-node state)]
    (if take-parent-by-class?
      (gd/getAncestorByClass node take-parent-by-class?)
      node)))

(defn get-dom-selection [state & [take-parent-by-class?]]
  (let [node (get-node state take-parent-by-class?)
        selection (.getSelection js/window)]
    (when (and
            selection
            (> (.-rangeCount selection) 0)
            (let [rng (.getRangeAt selection 0)]
              (gd/contains node (.-startContainer rng))))
      selection)))

(defn scroll-to-caret [node range]
  (let [start-node (.-startContainer range)
        start-node (if (gd/isElement start-node)
                     start-node
                     (gd/getAncestor
                       start-node
                       (fn [n] (gd/isElement n))
                       true))]
    (when (> (+ (.-offsetTop start-node) (.-offsetHeight start-node))
             (+ (.-scrollTop node) (.-offsetHeight node)))
      (.scrollIntoView start-node))))

(defn set-selection [state & [force?]]
  (let [node (-get-node state)
        {:keys [doc selection] :as opendoc} (c/cursor state 0)
        doc @doc
        {:keys [start end]} @selection
        reflected? (t/get-rendering-meta @opendoc :selection-reflected?)
        actual-selection #_(dom-selection->doc-selection doc (get-dom-selection state))
        (dom-selection->doc-selection-raw (get-dom-selection state))]
    #_(println "---setselection ---" actual-selection @selection
               #_(dom-selection->doc-selection-raw (get-dom-selection state))
               #_(not= @selection actual-selection)
               "\n" reflected?)
    ;;NOTE::snoop: we need this check to not destroy the selection direction
    (when (or force? (not reflected?) (not= @selection actual-selection))
      (let [new-range (.createRange js/document)
            start-node (doc-path->dom-node doc node start)]
        (.setStart new-range start-node (last start))
        (.setEnd new-range
                 (doc-path->dom-node doc node end)
                 (last end))
        (let [selection (.getSelection js/window)]
          (.removeAllRanges selection)
          (.addRange selection new-range))
        (scroll-to-caret node new-range)))
    (when-not (t/get-rendering-meta @opendoc :selection-reflected?)
      (c/swp! opendoc t/update-rendering-meta assoc :selection-reflected? true))))


(defn clipboard-keys []
  #{:key/c
    :key/v
    :key/x
    :key/a})

(defn history-keys []
  #{:key/y
    :key/z})

(defn nav-keys []
  #{:key/page-up
    :key/page-down
    :key/end
    :key/home
    :key/left-arrow
    :key/up-arrow
    :key/right-arrow
    :key/down-arrow})

(defn compose-set [& args]
  (set (apply concat
              (map (fn [a] (cond (fn? a) (a)
                                 (coll? a) a
                                 :else [a])) args))))

