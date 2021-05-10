(ns cryma.core.components.rte.rte-field
  (:require
    [cryma.core.components.cursors :as c]
    [cryma.core.components.ui-events :as uiev]
    [rum.core :as rum]
    [cryma.core.components.rte.model.configuration :as conf]
    [cryma.core.components.rte.model :as m]
    [cryma.core.components.rte.model.tools :as t]
    [cryma.core.components.rte.model.selection :as s]
    [cryma.core.components.rte.ui-tools :as uit]
    [cryma.core.components.rte.default-configuration :as default]
    [cryma.core.tools-ui :as tools-ui]
    [cryma.core.components.delayed :as delayed]
    [cryma.core.components.common :as common]
    [cryma.core.msg-filters :as mf]))

(rum/defcs
  doc-material
  < (c/cursored)
  [state material]
  (let [renderer (uit/semantic-renderer material)]
    (if renderer
      (rum/with-key (renderer material)
                    (uit/doc-cursor->key material))
      (throw (ex-info "Don't knowhow to render material"
                      {:material @material})))))

(rum/defcs
  doc-line
  < (c/cursored)
  [state line]
  [(uit/get-line-tag line)
   {:class        "rte-line"
    :data-keks-id (uit/doc-cursor->key line)
    :data-keks    (pr-str [(:type @line) (:semantics @line)])}
   (map
     (fn [{:keys [type] :as item}]
       (case @type
         :material (doc-material item)
         (throw (ex-info "Wrong line content item" {:line @line :item @item}))))
     (:content line))])

(rum/defcs
  doc-block
  < (c/cursored)
  [state block]
  (let [semantic-renderer (uit/semantic-renderer block)
        k (uit/doc-cursor->key block)]
    (if semantic-renderer
      (semantic-renderer block {:doc-block doc-block :doc-line doc-line})
      [(uit/get-tag block)
       {:data-keks-id k
        :data-keks    (pr-str [(:type @block) (:semantics @block)])}
       (map
         (fn [{:keys [type] :as item}]
           (case @type
             :block (rum/with-key (doc-block item) (uit/doc-cursor->key item))
             :line (rum/with-key (doc-line item) (uit/doc-cursor->key item))
             (throw (ex-info "Wrong block content item"
                             {:block @block :item @item}))))
         (:content block))])))

(defn flush-changes [state]
  (when-let [delayed (or (:__opendoc-mixin/delayed state)
                         (tools-ui/get-opts state :__opendoc-mixin/delayed))]
    (delayed/force-delayed delayed)))

;firefox workaround
(extend-type js/DOMStringList
  ISeqable
  (-seq [array] (array-seq array 0)))

(rum/defcs
  rte-field
  < (c/cursored {}
                {:external-key-down-handler {}})
    (merge
      {:will-mount
       (fn [state]
         (let [{:keys [key-down-spec]} (tools-ui/get-opts state)
               ext-key-down-handler (c/non-watchable state :external-key-down-handler)]
           (when key-down-spec
             (c/rst! ext-key-down-handler
                     (update key-down-spec :filter
                             (fn [filter]
                               (if (map? filter)
                                 (mf/msg-filter->fn filter)
                                 filter))))))
         state)
       :did-update
       (fn [state] (uit/set-selection state) state)
       :did-mount
       (fn [state]
         (let [opts (tools-ui/get-opts state)
               node (tools-ui/get-node state)]
           (when (:focused? opts) (.focus node))
           (uit/set-selection state))
         state)}
      common/mix-opts-should-update)
  [state cursor opts]
  (let [{:keys [doc] :as opendoc} (c/actual (c/cursor state 0))
        freezed? (or (get-in @opendoc [:ui :freezed?]) (:disabled? opts))
        ext-key-down-handler
        (c/actual (c/non-watchable state :external-key-down-handler))]
    [:div.rte
     {:ref           (fn [e] (when e (aset e "contentEditable" true)))
      :on-copy       (fn [e]
                       (uiev/cancel-event e)
                       (when-not freezed?
                         (when-let [dom-selection (uit/get-dom-selection state)]
                           (when-let [res
                                      (m/do-action
                                        (uit/acualize-selection @opendoc dom-selection)
                                        {:qn :doc.api/copy})]
                             (doseq [[k v] (:clipboard res)]
                               (.setData (.-clipboardData e)
                                         (str "text/" (name k)) v))))))
      :on-cut        (fn [e]
                       (uiev/cancel-event e)
                       (when-not freezed?
                         (when-let [dom-selection (uit/get-dom-selection state)]
                           (when-let [res
                                      (m/do-action
                                        (uit/acualize-selection @opendoc dom-selection)
                                        {:qn :doc.api/cut})]
                             (doseq [[k v] (:clipboard res)]
                               (.setData (.-clipboardData e)
                                         (str "text/" (name k)) v))
                             (c/rst! opendoc (dissoc res :clipboard))))))
      :on-paste      (fn [e]
                       (uiev/cancel-event e)
                       (when-not freezed?
                         (when-let [dom-selection (uit/get-dom-selection state)]
                           (let [clipboard-data (.-clipboardData e)
                                 clipboard
                                 (->>
                                   (.-types clipboard-data)
                                   (map
                                     (fn [t]
                                       (let [data (.getData clipboard-data t)]
                                         (when (and (string? data)
                                                    (seq (.trim data)))
                                           [(keyword t)
                                            (.getData clipboard-data t)]))))
                                   (into {}))]
                             (when (seq clipboard)
                               (c/swp!
                                 opendoc
                                 #(m/do-action
                                   (uit/acualize-selection % dom-selection)
                                   {:qn        :doc.api/paste
                                    :clipboard clipboard})))))))
      :on-drag-start uiev/cancel-event
      :on-drag       uiev/cancel-event
      :on-drag-end   uiev/cancel-event
      :on-key-down   (fn [e]
                       (if freezed?
                         (uiev/cancel-event e)
                         (let [ev (uiev/wrap-event e :on-key-down)
                               external-handler?
                               (and
                                 ext-key-down-handler
                                 (:filter @ext-key-down-handler)
                                 ((:filter @ext-key-down-handler) ev))
                               external-result?
                               (when external-handler?
                                 (flush-changes state)
                                 ((:handler @ext-key-down-handler) ev))]
                           (if (= external-result? :cancel-event)
                             (uiev/cancel-event e)
                             (case (:kk ev)
                               :key/y
                               (when (or (:meta? ev) (:ctrl? ev))
                                 (uiev/cancel-event e)
                                 (c/swp!
                                   opendoc
                                   m/do-action
                                   {:qn :doc.api/redo}))
                               :key/z
                               (when (or (:meta? ev) (:ctrl? ev))
                                 (uiev/cancel-event e)
                                 (c/swp!
                                   opendoc
                                   m/do-action
                                   {:qn (if (:shift? ev)
                                          :doc.api/redo
                                          :doc.api/undo)}))
                               :key/delete
                               (when-let [dom-selection (uit/get-dom-selection state)]
                                 (uiev/cancel-event e)
                                 (c/swp!
                                   opendoc
                                   #(m/do-action
                                     (uit/acualize-selection % dom-selection)
                                     {:qn :doc.api/del})))
                               :key/backspace
                               (when-let [dom-selection (uit/get-dom-selection state)]
                                 (uiev/cancel-event e)
                                 (c/swp!
                                   opendoc
                                   #(m/do-action
                                     (uit/acualize-selection % dom-selection)
                                     {:qn :doc.api/backspace})))
                               :key/enter
                               (when-let [dom-selection (uit/get-dom-selection state)]
                                 (uiev/cancel-event e)
                                 (c/swp!
                                   opendoc
                                   #(m/do-action
                                     (uit/acualize-selection % dom-selection)
                                     {:qn :doc.api/new-line})))

                               (when
                                 (and
                                   (not (:alt? ev))
                                   (or (:meta? ev) (:ctrl? ev))
                                   (not ((uit/compose-set
                                           nil
                                           uit/clipboard-keys
                                           uit/nav-keys)
                                          (:kk ev))))
                                 (.preventDefault e)))))))
      :on-key-press  (fn [e]
                       (let [ev (uiev/wrap-event e :on-key-press)]
                         (when-not (and (or (not (:meta? ev))
                                            (not (:ctrl? ev)))
                                        (#{99 118 120} (:char-code ev)))
                           (uiev/cancel-event e))
                         (when-not freezed?
                           (when-not (:meta? ev)
                             (case (:kk ev)
                               (let [dom-selection (uit/get-dom-selection state)]
                                 (c/swp!
                                   opendoc
                                   #(m/do-action
                                     (uit/acualize-selection % dom-selection)
                                     {:qn  :doc.api/type-in
                                      :format
                                           (get-in
                                             (uit/current-selection-state %)
                                             [:basic-formatting :format])
                                      :key (:key ev)}))))))))
      :on-key-up     (fn [e]
                       (if freezed?
                         (uiev/cancel-event e)
                         (let [ev (uiev/wrap-event e :on-key-up)]
                           (when ((uit/nav-keys) (:kk ev))
                             (when-let [dom-selection (uit/get-dom-selection state)]
                               (c/swp!
                                 opendoc uit/acualize-selection dom-selection :reflected!))))))
      :on-click      (fn [e]
                       (if freezed?
                         (uiev/cancel-event e)
                         (when-let [dom-selection (uit/get-dom-selection state)]
                           (c/swp!
                             opendoc uit/acualize-selection dom-selection :reflected!))))}
     (when (= @doc (t/create-empty-document))
       (let [conf (uit/cursor->conf opendoc)]
         (when (seq (:placeholder conf))
           [:span.ph
            {:content-editable false
             :on-focus         uiev/cancel-event
             :on-click         (fn [_] (uit/set-selection state :force!))}
            (:placeholder conf)])))
     (rum/with-key (doc-block doc) (uit/doc-cursor->key doc))]))

(defn opendoc-mixin [& [{:keys [non-watchable-opendoc-cursor-key
                                strdoc-constructor-index
                                docconf-options-key docconf
                                init-cursor-at]}]]
  (let [opendoc-key (or non-watchable-opendoc-cursor-key :opendoc)]
    {:will-mount
     (fn [state]
       (let [opendoc (c/local state opendoc-key)
             doc-str (c/cursor state (or strdoc-constructor-index 0))]
         (c/rst! opendoc (m/open-document
                           @doc-str
                           (conf/create-configuration
                             default/default-configuration
                             docconf
                             (tools-ui/get-opts
                               state (or docconf-options-key :doc/conf)))
                           (or init-cursor-at :end)))
         (let [conf (uit/cursor->conf opendoc)
               {:keys [import export]} (:converters conf)
               delayed (delayed/delayed-ctor
                         (get-in conf [:change-notification-timeout-ms]))]
           (add-watch
             (c/-src opendoc) [(:rum/id state) :__change-notification]
             (fn [_ _ old new]
               (let [doc-new (:doc new)]

                 (when (not= (:doc old) doc-new)
                   (delayed/delayed-fn-call
                     delayed (fn [] (c/rst! doc-str (export doc-new))))))))
           (add-watch
             (c/-src doc-str) [(:rum/id state) :__change-notification]
             (fn [_ _ _ new]
               (let [new-doc (import (get-in new (c/-path doc-str)))]
                 (when (not= new-doc (:doc @opendoc))
                   (c/swp! opendoc
                           (fn [od]
                             (let [new-doc (with-meta new-doc (meta (:doc @opendoc)))]
                               (-> od
                                   (assoc :doc new-doc)
                                   (s/update-selection
                                     (s/init-selection-end
                                       new-doc))
                                   (t/update-rendering-meta assoc :selection-reflected? false)))))))))
           (assoc state :__opendoc-mixin/delayed delayed))))

     :transfer-state
     (fn [old new]
       (assoc new :__opendoc-mixin/delayed (:__opendoc-mixin/delayed old)))

     :will-unmount
     (fn [state]
       (let [opendoc (c/local state opendoc-key)
             doc-str (c/cursor state (or strdoc-constructor-index 0))
             delayed (:__opendoc-mixin/delayed state)]
         (remove-watch (c/-src opendoc) [(:rum/id state) :__change-notification])
         (remove-watch (c/-src doc-str) [(:rum/id state) :__change-notification])
         (when delayed (delayed/cancel-delayed delayed)))
       state)}))
