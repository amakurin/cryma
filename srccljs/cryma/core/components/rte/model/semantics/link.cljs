(ns cryma.core.components.rte.model.semantics.link
  (:require
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [cryma.core.components.rte.model.configuration :as conf]
    [cryma.core.components.rte.model.semantics.protocols :as protocols]))

(defn original-semantics [conf node]
  (conf/get-semantics conf (get-in node [:meta :original-semantics]) :logic))

(defn link-material-semantics []
  (reify
    protocols/IGenericHtmlExportSupport
    (html-tag [_ _ _] :a)
    (html-attrs [_ node _] {:href (get-in node [:data :href])
                            :target "_blank"})
    protocols/IMaterialSemantics
    (join-nodes
      [_ start-node end-node conf]
      (protocols/join-nodes
        (original-semantics conf start-node) start-node end-node conf))
    (copy-selection
      [_ node start end inverse? conf]
      (protocols/copy-selection
        (original-semantics conf node) node start end inverse? conf))
    (count-positions
      [_ node conf]
      (protocols/count-positions
        (original-semantics conf node) node conf))
    (type-in
      [_ node position text conf]
      (protocols/type-in
        (original-semantics conf node) node position text conf))
    (deny-type-in?
      [_ node position text conf]
      (or (re-find #"[\s\n\t]" text)
          (protocols/deny-type-in?
            (original-semantics conf node) node position text conf)))
    (deny-wrap? [_ _ _] true)
    (to-text
      [_ node conf]
      (protocols/to-text
        (original-semantics conf node) node conf))
    (html-content
      [_ node conf]
      (let [semantics (original-semantics conf node)]
        (protocols/html-content semantics node conf)))))