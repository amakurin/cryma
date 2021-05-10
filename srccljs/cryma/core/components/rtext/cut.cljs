(ns cryma.core.components.rtext.cut
  (:require
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    #_[cryma.core.l10n :refer [l]]
    [cryma.core.components.cursors :as c]
    [cryma.core.components.rte.ui-tools :as uit]
    [cryma.core.components.rte.rte-toolbox :as rtetb]
    [cryma.core.components.rte.model.semantics.protocols :as semantics]
    [cryma.core.components.rte.model.tools :as t]
    [cryma.core.components.rte.model.selection :as s]
    [cryma.core.components.rte.model.convertation :as conv]
    [cryma.core.components.rte.model.configuration :as conf]
    [cryma.core.components.rte.model :as m]
    [rum.core :as rum]
    [clojure.zip :as z]))

(defn prepare-cut [s]
  (or s ""))

(rum/defcs
  cut-dialog
  < (c/cursored)
  [state module context data]
  [:div.dialog-cut
   [:textarea
    {:placeholder ((:localizer module) :rte/cut-tool-dialog-text-placeholder)
     :on-key-down (fn [e] (.stopPropagation e))
     :on-change   (fn [e] (c/swp! data assoc-in [:content] (.. e -target -value)))
     :value       (get-in @data [:content])}]])

(defn has-cut? [doc]
  (loop [z (z/next (t/doc-zipper doc))]
    (cond
      (= (t/zip-semantics z) :cut)
      true
      (t/zip-rightmost? z)
      false
      :else
      (recur (z/right z)))))

(defn cut-tool [module]
  (rtetb/dialog-tool
    {:tool-name         :cut
     :active-check      {:block-formatting {:semantics :cut}}
     :disabled-check-fn (fn [opendoc _]
                          (and
                            (not= :cut
                                  (uit/current-selection-val
                                    opendoc [:block-formatting :semantics]))
                            (or (uit/current-selection-val opendoc [:multiline?])
                                (has-cut? (:doc opendoc)))))
     :dialog-title-key  :rte/cut-tool-dialog-header
     :dialog-component  (partial cut-dialog module)
     :dialog-init-data-fn
                        (fn [opendoc]
                          (let [{:keys [semantics] :as current-block}
                                (uit/current-selection-val @opendoc [:current-block])
                                update? (= :cut semantics)
                                selected-text
                                (cond
                                  update?
                                  (conv/get-text (t/doc-zipper
                                                   (conf/with-conf
                                                     current-block
                                                     (uit/cursor->conf opendoc))))
                                  (not (uit/current-selection-val
                                         @opendoc [:collapsed?]))
                                  (->
                                    (t/doc-zipper (:doc @opendoc))
                                    (s/zip-copy-selection-minimized
                                      (:selection @opendoc))
                                    conv/get-text))]

                            {:type      :block
                             :semantics (when update? :cut)
                             :content   (if (empty? selected-text)
                                          ((:localizer module)
                                            :rte/cut-tool-initial-text) selected-text)}))
     :dialog-validation-fn
                        (fn [dialog-context]
                          (let [cut (prepare-cut
                                      (:content @(:data dialog-context)))]
                            (when-not (seq cut)
                              [{:field :cut
                                :code  :rte/cut-tool-text-required}])))
     :dialog-result-fn  (fn [{:keys [button-key dialog-context]}]
                          (cond
                            (= button-key :ok)
                            (let [{:keys [opendoc _ _ data]} dialog-context
                                  {:keys [semantics content] :as current} @data
                                  update? (and current (= :cut semantics))]
                              (c/swp!
                                opendoc
                                m/do-action
                                {:qn   (if update? :doc.api/update :doc.api/insert)
                                 :item {:type      :block
                                        :semantics :cut
                                        :content   [(assoc-in
                                                      (t/create-empty-line)
                                                      [:content 0 :content]
                                                      (prepare-cut content))]}}))))}))

(rum/defcs
  doc-cut
  < (c/cursored)
  [state module block {:keys [doc-block doc-line]}]
  (let [k (uit/doc-cursor->key block)]
    [:div.cut
     {:data-keks-id k}
     (map
       (fn [{:keys [type] :as item}]
         (case @type
           :block (rum/with-key (doc-block item) (uit/doc-cursor->key item))
           :line (rum/with-key (doc-line item) (uit/doc-cursor->key item))
           (throw (ex-info "Wrong block content item"
                           {:block @block :item @item}))))
       (:content block))]))

(defn cut-block-semantics [module]
  (reify
    semantics/IGenericHtmlExportSupport
    (html-tag [_ _ _] :a)
    (html-attrs [_ _ _] {:id "cut"
                         :class "cut"})
    semantics/IBlockSemantics
    (html-line-tag [_ _ _])
    (allow-new-line? [_ _ _ _] false)))