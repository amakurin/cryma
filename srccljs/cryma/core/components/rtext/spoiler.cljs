(ns cryma.core.components.rtext.spoiler
  (:require
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [cryma.core.components.rte.model.semantics.protocols :as protocols]
    [cryma.core.components.rte.model.semantics.blocks :as blocks]
    [cryma.core.components.cursors :as c]
    [cryma.core.components.rte.ui-tools :as uit]
    #_[cryma.core.l10n :refer [l]]
    [rum.core :as rum]))

(rum/defcs
  doc-spoiler
  < (c/cursored)
  [state module block {:keys [doc-block doc-line]}]
  (let [l (:localizer module)
        k (uit/doc-cursor->key block)]
    [:div.spoiler
     {:data-keks-id k}
     [:span.title (l :app.post/spoiler-default-title)]
     [:div.content
      (map
       (fn [{:keys [type] :as item}]
         (case @type
           :block (rum/with-key (doc-block item) (uit/doc-cursor->key item))
           :line (rum/with-key (doc-line item) (uit/doc-cursor->key item))
           (throw (ex-info "Wrong block content item"
                           {:block @block :item @item}))))
       (:content block))]]))

(defn spoiler-block-semantics []
  (reify
    protocols/IGenericHtmlExportSupport
    (html-tag [_ _ _] :div)
    (html-attrs [_ _ _] {:class "spoiler"})
    protocols/IBlockSemantics
    (html-line-tag [_ _ _])
    (allow-new-line? [_ _ line-z _]
      (blocks/-allow-new-line? line-z))))
