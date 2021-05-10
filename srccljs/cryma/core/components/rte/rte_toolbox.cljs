(ns cryma.core.components.rte.rte-toolbox
  (:require
    [cryma.core.components.cursors :as c]
    [cryma.core.components.ui-events :as uiev]
    [goog.dom :as gd]
    [clojure.string :as clostr]
    [rum.core :as rum]
    [cryma.core.components.rte.model :as m]
    [cryma.core.components.rte.model.selection :as s]
    [cryma.core.tools-ui :as tools-ui]
    [cryma.core.components.rte.model.configuration :as conf]
    [cryma.core.components.rte.ui-tools :as uit]
    [clojure.string :as clostr]
    [cryma.core.components.common :as common]))

(defn get-localizer [cursor-or-map]
  (let [m (if (c/cur? cursor-or-map) @cursor-or-map cursor-or-map)
        doc (if-let [doc (:doc m)] doc m)]
    (:localizer (conf/get-conf doc))))

(defn l [cursor-or-map v]
  (when-let [localizer (get-localizer cursor-or-map)]
    (localizer v)))

(defn active-tool-dialog? [opendoc]
  (get-in @opendoc [:ui :show-dialog]))

(defn deep-eq? [src chk]
  (loop [chk chk]
    (if-let [[k v] (first chk)]
      (when (-contains-key? src k)
        (let [src-v (get src k)]
          (when (cond
                  (and (map? src-v) (map? v))
                  (deep-eq? src-v v)
                  (and (set? src-v) (coll? v))
                  (every? src-v v)
                  :else (= src-v v))
            (recur (rest chk)))))
      true)))

(defn tool-active? [{:keys [selection-state selection-state-override] :as opendoc}
                    {:keys [active-check] :as tool}]
  (cond
    (fn? active-check) (active-check opendoc)
    (map? active-check)
    (deep-eq?
      (or selection-state-override
          selection-state) active-check)
    :else
    (throw (ex-info "Unknown active-check for rte-tool" {:tool tool}))))

(defn merge-check-to-selection-state [selection-state active-check]
  (merge-with
    (fn merge-vals [resv checkv]
      (cond
        (and (map? resv) (map? checkv))
        (merge-with merge-vals resv checkv)
        (and (set? resv) (set? checkv))
        (if (some resv checkv)
          (set (remove checkv resv))
          (set (concat resv checkv)))
        :else
        checkv))
    selection-state active-check))

(defn do-collapsed-strategy [_ opendoc tool]
  (assoc opendoc
    :selection-state-override
    (merge-check-to-selection-state
      (uit/current-selection-state opendoc)
      (:active-check tool))))

(defn focus-rte [node]
  (let [predicate (fn [node]
                    (and node
                         (gd/isElement node)
                         (.-className node)
                         (re-find #"^rte\s*$" (.-className node))))]
    (if (predicate node)
      (.focus node)
      (when-let [node (gd/findNode node predicate)]
        (.focus node)))))

(defn generic-executor [opendoc-cursor tool dom-selection]
  (c/swp!
    opendoc-cursor
    #(let [opendoc (uit/acualize-selection % dom-selection)]
      (if-let [collapsed-strategy
               (and (s/selection-collapsed? (:selection opendoc))
                    (:collapsed-strategy tool))]
        (do-collapsed-strategy collapsed-strategy opendoc tool)
        (m/do-action
          opendoc
          (if (tool-active? % tool)
            (:deactivate-action tool)
            (:activate-action tool)))))))

(defn execute-tool [state opendoc-cursor tool & [e]]
  (let [container-class (tools-ui/get-opts state :container-class)
        node (uit/get-node state container-class)]
    (when e (uiev/cancel-event e))
    (focus-rte node)
    (when-let [dom-selection (uit/get-dom-selection state container-class)]
      (if-let [executor (:execute-tool-fn tool)]
        (executor opendoc-cursor tool dom-selection)
        (generic-executor opendoc-cursor tool dom-selection)))))

(rum/defcs
  rte-tool-button
  < (c/cursored)
  common/mix-opts-should-update
  [state opendoc tool opts]
  (let [{:keys [key class title-fn disabled-check-fn shortcut-key]} tool
        active? (tool-active? @opendoc tool)
        disabled? (or
                    (:disabled? opts)
                    (when disabled-check-fn (disabled-check-fn @opendoc tool)))]
    [:li
     {:key           key
      :class         (str class
                          (when active? " active")
                          (when disabled? " disabled"))
      :title         (str (title-fn opendoc tool)
                          (when shortcut-key (str " CTRL(CMD)-"
                                                  (-> shortcut-key
                                                      name
                                                      clostr/upper-case))))
      :on-drag-start uiev/cancel-event
      :on-drag       uiev/cancel-event
      :on-click      (fn [e]
                       (when-not disabled?
                         (execute-tool state opendoc tool e)))}]))

(rum/defcs
  rte-toolbox
  < (c/cursored)
  common/mix-opts-should-update
  [state opendoc tools opts]
  [:div.toolbox
   {:on-mouse-move uiev/cancel-event}
   [:ul.tb-buttons
    (map #(rte-tool-button opendoc % opts) tools)]])


(defn editor-tool [{:keys [tool-name
                           active-check
                           disabled-check-fn
                           activate-action
                           deactivate-action
                           shortcut-key collapsed-strategy execute-tool-fn]}]
  (let [tool-name-str (name tool-name)]
    {:tool-name          tool-name
     :key                tool-name-str
     :class              (str "tbb-" tool-name-str)
     :title-fn           (fn [opendoc _]
                           (l opendoc (keyword "rte" (str "toolbutton-" tool-name-str))))
     :active-check       active-check
     :disabled-check-fn  disabled-check-fn
     :collapsed-strategy collapsed-strategy
     :activate-action    activate-action
     :deactivate-action  deactivate-action
     :shortcut-key       shortcut-key
     :execute-tool-fn    execute-tool-fn}))

(defn basic-formatting-tool [tool-name & [shortcut-key]]
  (editor-tool {:tool-name          tool-name
                :active-check       {:basic-formatting {:format #{tool-name}}}
                :activate-action    {:qn         :doc.api/basic-formatting
                                     :add-format #{tool-name}}
                :deactivate-action  {:qn            :doc.api/basic-formatting
                                     :remove-format #{tool-name}}
                :shortcut-key       shortcut-key
                :collapsed-strategy :merge-selection-state}))

(defn block-tool-param [semantics format]
  (merge
    {:semantics semantics}
    (when format
      {:format #{format}})))

(defn block-tool-action [add-or-remove semantics format]
  {:qn           :doc.api/block-formatting
   add-or-remove (block-tool-param semantics format)})

(defn block-formatting-tool [semantics & [format shortcut-key]]
  (editor-tool {:tool-name         (if format format semantics)
                :active-check      {:block-formatting (block-tool-param semantics format)}
                :activate-action   (block-tool-action :add-format semantics format)
                :deactivate-action (block-tool-action :remove-format semantics format)
                :shortcut-key      shortcut-key}))

(defn shortcut->tool [tools kk]
  (when kk
    (->>
      tools
      (filter (fn [t] (= kk (:shortcut-key t))))
      first)))

(defn dialog-button [button-key & [tool-predicate]]
  {:button-key     button-key
   :tool-predicate tool-predicate})

(defn close-tool-dialog [opendoc]
  (c/swp! opendoc update :ui dissoc :freezed? :show-dialog))

(rum/defcs
  rte-dialog-component-wrapper
  < (c/cursored)
  [state dialog-component context data]
  [:div.dialog-content-wrapper
   [:div.dialog-content (dialog-component context data)]
   (when-let [errors (:errors @data)]
     [:ul.err-desc
      (map
        (fn [error]
          [:li (l (:opendoc context) (:code error))])
        errors)])])

(rum/defcs
  rte-tool-dialog
  [state opendoc]
  (let [{:keys [dialog-header
                dialog-component
                dialog-context
                dialog-result-buttons
                dialog-validation-fn
                dialog-result-fn]} (active-tool-dialog? opendoc)]
    [:div.dialog-overlay
     {:on-click (fn [e]
                  (uiev/cancel-event e)
                  (close-tool-dialog opendoc))}
     [:div.placeholder
      {:on-click uiev/cancel-event}
      [:div.dialog
       [:div.dialog-header dialog-header]
       (rte-dialog-component-wrapper
         dialog-component dialog-context (:data dialog-context))
       [:ul.dialog-buttons
        (->>
          dialog-result-buttons
          (filter (fn [dialog-button]
                    (if-let [tool-predicate (:tool-predicate dialog-button)]
                      (tool-predicate @opendoc (get-in @opendoc [:ui :dialog-tool]))
                      true)))
          (map
            (fn [{:keys [button-key]}]
              [:li.btn
               {:on-click (fn [e]
                            (uiev/cancel-event e)
                            (let [errors (and (= button-key :ok)
                                              dialog-validation-fn
                                              (dialog-validation-fn dialog-context))]
                              (when-not errors
                                (when dialog-result-fn
                                  (dialog-result-fn {:button-key     button-key
                                                     :dialog-context dialog-context}))
                                (close-tool-dialog opendoc))
                              (when errors
                                (c/swp! (:data dialog-context)
                                        assoc :errors errors))))}
               (l opendoc
                  (keyword "rte" (str "dialog-result-button-" (name button-key))))])))]]]]))


(defn create-dialog [{:keys [opendoc tool dom-selection
                             dialog-title dialog-component dialog-initial-data
                             dialog-result-fn dialog-validation-fn additional-buttons]}]
  {:dialog-header        dialog-title
   :dialog-component     dialog-component
   :dialog-context       {:data          (c/cur dialog-initial-data)
                          :opendoc       opendoc
                          :tool          tool
                          :dom-selection dom-selection}
   :dialog-validation-fn dialog-validation-fn
   :dialog-result-buttons
                         (vec
                           (concat
                             additional-buttons
                             [(dialog-button :cancel)
                              (dialog-button :ok)]))
   :dialog-result-fn     dialog-result-fn})

(defn dialog-tool [{:keys [tool-name
                           active-check
                           disabled-check-fn
                           dialog-title-key
                           dialog-component
                           dialog-init-data-fn
                           dialog-validation-fn
                           dialog-result-fn
                           additional-buttons
                           shortcut-key] :as dt}]
  (editor-tool
    {:tool-name         tool-name
     :active-check      active-check
     :disabled-check-fn disabled-check-fn
     :shortcut-key      shortcut-key
     :execute-tool-fn   (fn [opendoc tool dom-selection]
                          (let [init-data (or dialog-init-data-fn
                                              (fn [opendoc]
                                                (uit/current-selection-val
                                                  @opendoc [:current-material])))]
                            (c/swp!
                              opendoc
                              assoc :ui
                              {:dialog-tool dt
                               :show-dialog
                                            (create-dialog
                                              {:opendoc              opendoc
                                               :tool                 tool
                                               :dom-selection        dom-selection
                                               :dialog-title         (l opendoc dialog-title-key)
                                               :dialog-component     dialog-component
                                               :dialog-initial-data  (init-data opendoc)
                                               :dialog-validation-fn dialog-validation-fn
                                               :dialog-result-fn     dialog-result-fn
                                               :additional-buttons   additional-buttons})
                               :freezed?    true})))}))


(rum/defcs
  link-dialog
  < (c/cursored)
  [state context data]
  [:div.dialog-link
   [:textarea
    {:placeholder (l (:opendoc context) :rte/link-tool-dialog-url-placeholder)
     :on-key-down (fn [e] (.stopPropagation e))
     :on-change   (fn [e] (c/swp! data assoc-in [:data :href] (.. e -target -value)))
     :value       (get-in @data [:data :href])}]])

(defn prepare-href [s]
  (->>
    (or s "")
    clostr/trim
    (re-find #"(?im)^(https?://|ftp://|www\.)[^\s]+$")
    first))

(defn link-tool []
  (dialog-tool
    {:tool-name        :link
     :active-check     {:basic-formatting {:semantics :link}
                        :multimaterial?   false}
     :disabled-check-fn
                       (fn [opendoc _] (uit/current-selection-val opendoc [:multiline?]))
     :dialog-title-key :rte/link-tool-dialog-header
     :dialog-component link-dialog
     :dialog-validation-fn
                       (fn [dialog-context]
                         (let [url (prepare-href (get-in @(:data dialog-context) [:data :href]))]
                           (when-not url
                             [{:field :href
                               :code  :rte/link-tool-url-required}])))
     :dialog-result-fn
                       (fn [{:keys [button-key dialog-context]}]
                         (cond
                           (= button-key :ok)
                           (let [{:keys [opendoc _ _ data]} dialog-context
                                 {:keys [semantics data] :as current-material} @data
                                 update? (and current-material (= :link semantics))
                                 prepared-data (update data :href prepare-href)]
                             (c/swp!
                               opendoc
                               m/do-action
                               {:qn              (if update? :doc.api/update :doc.api/insert)
                                :item            {:type      :material
                                                  :semantics :link
                                                  :data      prepared-data
                                                  :content   (when (uit/current-selection-val
                                                                     @opendoc [:collapsed?])
                                                               (:href prepared-data))}
                                :wrap-selection? :wrap-selection!}))
                           (= button-key :remove-link)
                           (let [{:keys [opendoc _ _ data]} dialog-context
                                 {:keys [data meta]} @data]
                             (c/swp!
                               opendoc
                               m/do-action
                               {:qn   :doc.api/update
                                :item {:type      :material
                                       :semantics (:original-semantics meta)
                                       :data      (dissoc data :href)
                                       :meta      (dissoc meta :original-semantics)}}))))
     :additional-buttons
                       [(dialog-button :remove-link tool-active?)]}))

(rum/defcs
  image-dialog
  < (c/cursored)
  [state context data]
  [:div.dialog-img
   [:textarea
    {:placeholder (l (:opendoc context) :rte/image-tool-dialog-url-placeholder)
     :on-key-down (fn [e] (.stopPropagation e))
     :on-change   (fn [e] (c/swp! data assoc-in [:data :src] (.. e -target -value)))
     :value       (get-in @data [:data :src])}]])

(defn prepare-src [s]
  (->>
    (or s "")
    clostr/trim
    (re-find #"(?im)^(https?://|data:image/|ftp://|www\.)[^\s]+$")
    first))

(defn image-tool []
  (dialog-tool
    {:tool-name        :image
     :active-check     {:basic-formatting {:semantics :image}
                        :multimaterial?   false}
     :dialog-title-key :rte/image-tool-dialog-header
     :dialog-component image-dialog
     :dialog-validation-fn
                       (fn [dialog-context]
                         (let [url (prepare-src (get-in @(:data dialog-context) [:data :src]))]
                           (when-not url
                             [{:field :src
                               :code  :rte/image-tool-url-required}])))
     :dialog-result-fn (fn [{:keys [button-key dialog-context]}]
                         (when (= button-key :ok)
                           (let [{:keys [opendoc _ _ data]} dialog-context
                                 {:keys [data] :as current-material} @data
                                 update? (and current-material
                                              (= :image (:semantics current-material)))]
                             (c/swp!
                               opendoc
                               m/do-action
                               {:qn   (if update? :doc.api/update :doc.api/insert)
                                :item {:type      :material
                                       :semantics :image
                                       :data      (update
                                                    data :src
                                                    prepare-src)}}))))}))

(rum/defcs
  video-dialog
  < (c/cursored)
  [state context data]
  [:div.dialog-video
   [:textarea
    {:placeholder (l (:opendoc context) :rte/video-tool-dialog-url-placeholder)
     :on-key-down (fn [e] (.stopPropagation e))
     :on-change   (fn [e] (c/swp! data assoc-in [:data :embedding-code] (.. e -target -value)))
     :value       (get-in @data [:data :embedding-code])}]])

(defn prepare-embedding-code [s]
  (->>
    (or s "")
    clostr/trim
    (re-find #"(?im)^<iframe.+</iframe>$")))

(defn video-tool []
  (dialog-tool
    {:tool-name        :video
     :active-check     {:basic-formatting {:semantics :video}
                        :multimaterial?   false}
     :dialog-title-key :rte/video-tool-dialog-header
     :dialog-component video-dialog
     :dialog-init-data-fn
                       (fn [opendoc]
                         (uit/current-selection-val @opendoc [:basic-formatting :data]))
     :dialog-validation-fn
                       (fn [dialog-context]
                         (let [embedding-code (prepare-embedding-code
                                                (get-in @(:data dialog-context)
                                                        [:data :embedding-code]))]
                           (when-not embedding-code
                             [{:field :embedding-code
                               :code  :rte/video-tool-embedding-code-required}])))
     :dialog-result-fn
                       (fn [{:keys [button-key dialog-context]}]
                         (when (= button-key :ok)
                           (let [{:keys [opendoc _ _ data]} dialog-context
                                 {:keys [semantics data] :as current-material} @data
                                 update? (and current-material
                                              (= :image semantics))]
                             (c/swp!
                               opendoc
                               m/do-action
                               {:qn   (if update? :doc.api/update :doc.api/insert)
                                :item {:type      :material
                                       :semantics :video
                                       :data      (update
                                                    data :embedding-code
                                                    prepare-embedding-code)}}))))}))
