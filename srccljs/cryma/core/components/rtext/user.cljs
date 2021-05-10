(ns cryma.core.components.rtext.user
  (:require
    [cryma.core.components.cursors :as c]
    [cryma.core.components.rte.ui-tools :as uit]
    [cryma.core.components.rte.rte-toolbox :as rtetb]
    [cryma.core.components.rte.model.semantics.protocols :as semantics]
    [cryma.core.components.rte.materials :as materials]
    [cryma.core.components.rte.model.tools :as t]
    [cryma.core.components.rte.model.semantics.text :as text-semantics]
    [clojure.string :as clostr]
    [cryma.core.components.rte.model :as m]
    [cryma.core.components.rte.model.autocorrectors.protocols :as autocorrect]
    [cryma.core.components.rte.model.selection :as s]
    [cryma.core.components.rte.model.convertation :as conv]
    [rum.core :as rum]
    [clojure.zip :as z]
    [cryma.core.tools-ui :as tools-ui]))

(defn prepare-username [s]
  (->>
    (or s "")
    (str "@")
    clostr/trim
    (re-find #"(?im)^@[\w\-]+$")))

(defn user-uri [module username]
  (tools-ui/event->route
    module
    {:qn     :action.app/read-user
     :filter {:db/id
              (->
                username
                clostr/trim
                (clostr/replace #"@" "-"))}}))

(defn user-material-semantics [module]
  (let [text-semantics (text-semantics/text-material-semantics)]
    (reify
      semantics/IGenericHtmlExportSupport
      (html-tag [_ _ _] :a)
      (html-attrs [_ node _] {:href   (user-uri module (:content node))
                              :target "_blank"
                              :class  "user"})
      semantics/IMaterialSemantics
      (join-nodes
        [_ start-node end-node conf]
        (semantics/join-nodes text-semantics start-node end-node conf))
      (copy-selection
        [_ node start end inverse? _]
        (let [content (:content node)
              new-content (if inverse?
                            (str
                              (when start (subs content 0 start))
                              (when end (subs content end)))
                            (subs content
                                  (or start 0)
                                  (or end (count content))))]
          (if (clostr/includes? new-content "@")
            (assoc node :content new-content)
            (assoc (t/create-empty-text-material) :content new-content))))
      (count-positions
        [_ node conf]
        (semantics/count-positions text-semantics node conf))
      (type-in
        [_ node position text conf]
        (semantics/type-in text-semantics node position text conf))
      (deny-type-in? [_ _ position text _] (or (= 0 position) (re-find #"[^\w\-]" text)))
      (deny-wrap? [_ _ _] false)
      (to-text [_ node conf] (semantics/to-text text-semantics node conf))
      (html-content [_ node conf] (semantics/html-content text-semantics node conf)))))

(rum/defcs
  user-dialog
  < (c/cursored)
  [state module context data]
  [:div.dialog-user
   [:textarea
    {:placeholder ((:localizer module) :rte/user-tool-dialog-name-placeholder)
     :on-key-down (fn [e] (.stopPropagation e))
     :on-change   (fn [e] (c/swp! data assoc-in [:content] (.. e -target -value)))
     :value       (get-in @data [:content])}]])

(defn user-tool [module]
  (rtetb/dialog-tool
    {:tool-name         :user
     :active-check      {:basic-formatting {:semantics :user}
                         :multimaterial?   false}
     :disabled-check-fn (fn [opendoc _] (uit/current-selection-val opendoc [:multiline?]))
     :dialog-title-key  :rte/user-tool-dialog-header
     :dialog-component  (partial user-dialog module)
     :dialog-init-data-fn
                        (fn [opendoc]
                          (let [{:keys [semantics] :as current-material}
                                (uit/current-selection-val @opendoc [:current-material])]
                            (if (= :user semantics)
                              (update current-material :content
                                      clostr/replace #"@" "")
                              {:content (->
                                          (t/doc-zipper (:doc @opendoc))
                                          (s/zip-copy-selection-minimized
                                            (:selection @opendoc))
                                          conv/get-text)})))
     :dialog-validation-fn
                        (fn [dialog-context]
                          (let [username (prepare-username
                                           (:content @(:data dialog-context)))]
                            (when-not username
                              [{:field :name
                                :code  :rte/user-tool-name-required}])))
     :dialog-result-fn  (fn [{:keys [button-key dialog-context]}]
                          (cond
                            (= button-key :ok)
                            (let [{:keys [opendoc _ _ data]} dialog-context
                                  {:keys [semantics content] :as current-material} @data
                                  update? (and current-material (= :user semantics))]
                              (c/swp!
                                opendoc
                                m/do-action
                                {:qn   (if update? :doc.api/update :doc.api/insert)
                                 :item {:type      :material
                                        :semantics :user
                                        :content   (prepare-username content)}}))))}))

(rum/defcs
  doc-user
  < (c/cursored)
  [state module material]
  (let [{:keys [content]} @material]
    [:a.rte-material
     {:href         (user-uri module content)
      :target       "_blank"
      :class        "user"
      :data-keks-id (uit/doc-cursor->key material)}
     (materials/render-string content)]))

(defn positions-to-split [s]
  (->>
    s
    (re-find #"(?im)@[\w\-]+")
    ((fn [found]
       (when found
         (let [start-pos (clostr/index-of s found)]
           [start-pos (+ start-pos (count found)) found]))))))

(defn user-autocorrector []
  (reify
    autocorrect/IAutoCorrector
    (applicable? [_ opendoc {:keys [qn] :as ev}]
      (and
        (s/selection-collapsed? (:selection opendoc))
        (and (= qn :doc.api/type-in) (re-find #"[^\w\-]" (:key ev)))))
    (correct [_ opendoc]
      (let [z (autocorrect/zip-to-selection opendoc)]
        (if (and (t/zip-is-material? z) (= (t/zip-semantics z) :text))
          (let [end-pos (peek (t/zip-end-path z))
                to-search (subs (:content (z/node z)) 0 end-pos)
                [start end username] (positions-to-split to-search)]
            (if start
              (let [node (z/node z)
                    text-node-left (when (> start 0) (update node :content subs 0 start))
                    link-node (-> node
                                  (assoc :semantics :user)
                                  (assoc :content username))
                    text-node-right (update node :content subs end)
                    z (if text-node-left (z/insert-left z text-node-left) z)
                    z (z/insert-left z link-node)
                    z (z/replace z (t/set-material-position text-node-right (- end-pos end)))]
                (-> opendoc
                    (assoc :doc (z/root z))
                    (assoc :selection (s/collapsed-selection (t/zip-full-path z)))))
              opendoc))
          opendoc)))))
