(ns cryma.core.components.rte.model.semantics.media
  (:require
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [cryma.core.components.rte.model.semantics.protocols :as protocols]))

(defn media-copy-selection [node start end inverse? _]
  (cond
    (and inverse?
         (or (and (or (nil? start) (= start 0))
                  (or (nil? end) (> end 0)))))
    nil
    inverse? node
    (or (and (or (nil? start) (= start 0))
             (or (nil? end) (> end 0))))
    node
    :else nil))

;;-----------------------------
;; IMAGE
;;-----------------------------
(defn image-material-semantics []
  (reify
    protocols/IGenericHtmlExportSupport
    (html-tag [_ _ _] :img)
    (html-attrs [_ node _] {:src (get-in node [:data :src])})
    protocols/IMaterialSemantics
    (join-nodes
      [_ start-node end-node _] [start-node end-node])
    (copy-selection
      [_ node start end inverse? conf] (media-copy-selection node start end inverse? conf))
    (count-positions
      [_ _ _] 1)
    (type-in
      [_ _ _ _ _])
    (deny-type-in? [_ _ _ _ _] true)
    (deny-wrap? [_ _ _] false)
    (to-text [_ _ _] "")
    (html-content [_ _ _] [])))

;;-----------------------------
;; VIDEO
;;-----------------------------
(defn video-material-semantics []
  (reify
    protocols/IGenericHtmlExportSupport
    (html-tag [_ _ _] :video)
    (html-attrs [_ _ _])
    protocols/IMaterialSemantics
    (join-nodes
      [_ start-node end-node _] [start-node end-node])
    (copy-selection
      [_ node start end inverse? conf] (media-copy-selection node start end inverse? conf))
    (count-positions
      [_ _ _] 1)
    (type-in
      [_ _ _ _ _])
    (deny-type-in? [_ _ _ _ _] true)
    (deny-wrap? [_ _ _] true)
    (to-text [_ _ _] "")
    (html-content [_ _ _] [])))

