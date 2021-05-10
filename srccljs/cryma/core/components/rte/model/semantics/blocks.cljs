(ns cryma.core.components.rte.model.semantics.blocks
  (:require
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [cryma.core.components.rte.model.semantics.protocols :as protocols]
    [cryma.core.components.rte.model.tools :as t]))

(defn -allow-new-line? [line-z]
  (not (and (t/-empty? line-z)
            (> (t/zip-branch-pos line-z) 0))))

(defn document-block-semantics [& [tag line-tag]]
  (let [tag (or tag :div)
        line-tag (or line-tag :p)]
    (reify
      protocols/IGenericHtmlExportSupport
      (html-tag [_ _ _] tag)
      (html-attrs [_ _ _])
      protocols/IBlockSemantics
      (html-line-tag [_ node _] (if (= node (t/create-empty-line))
                                  :br line-tag))
      (allow-new-line? [_ _ _ _] true))))

(defn generic-block-semantics [tag & [line-tag]]
  (reify
    protocols/IGenericHtmlExportSupport
    (html-tag [_ _ _] tag)
    (html-attrs [_ _ _])
    protocols/IBlockSemantics
    (html-line-tag [_ _ _] (when line-tag (fn [_] line-tag)))
    (allow-new-line? [_ _ line-z _] (-allow-new-line? line-z))))

(defn list-block-semantics []
  (reify
    protocols/IGenericHtmlExportSupport
    (html-tag [_ node _]
      (if (and (:format node) ((:format node) :ordered-list)) :ol :ul))
    (html-attrs [_ _ _])
    protocols/IBlockSemantics
    (html-line-tag [_ _ _] :li)
    (allow-new-line? [_ _ line-z _] (-allow-new-line? line-z))))
