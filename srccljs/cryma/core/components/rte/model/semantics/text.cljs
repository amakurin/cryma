(ns cryma.core.components.rte.model.semantics.text
  (:require
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [cryma.core.components.rte.model.semantics.protocols :as protocols]))

(defn text-material-semantics []
  (reify
    protocols/IGenericHtmlExportSupport
    (html-tag [_ _ _] :span)
    (html-attrs [_ _ _])
    protocols/IMaterialSemantics
    (join-nodes
      [_ start-node end-node _]
      [(update start-node :content
               (fn [start-text] (str start-text (:content end-node))))])
    (copy-selection
      [_ node start end inverse? _]
      (update node :content
              (fn [content]
                (if inverse?
                  (str
                    (when start (subs content 0 start))
                    (when end (subs content end)))
                  (subs content
                        (or start 0)
                        (or end (count content)))))))
    (count-positions
      [_ node _] (-> node :content count))
    (type-in
      [_ node position text _]
      {:item     (update node :content
                         (fn [content]
                           (str
                             (subs content 0 position)
                             text
                             (subs content position))))
       :position (+ position (count text))})
    (deny-type-in? [_ _ _ _ _] false)
    (deny-wrap? [_ _ _] false)
    (to-text [_ node _] (:content node))
    (html-content [_ node _] [(:content node)])))

