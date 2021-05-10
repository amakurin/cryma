(ns cryma.core.components.rte.rte-full
  (:require
    [cryma.core.components.ui-events :as uiev]
    [cryma.core.components.cursors :as c]
    [rum.core :as rum]
    [cryma.core.components.rte.ui-tools :as uit]
    [cryma.core.components.rte.rte-field :as rtef]
    [cryma.core.components.rte.rte-toolbox :as rtetb]
    [cryma.core.tools-ui :as tools-ui]
    [cryma.core.components.common :as common]))

(rum/defcs
  rte-full
  < (c/cursored
      {:opendoc {:doc nil}
       :local   {:focused? false}}
      {:tools [(rtetb/block-formatting-tool :h2)
               (rtetb/block-formatting-tool :h3)
               (rtetb/block-formatting-tool :h4)
               (rtetb/basic-formatting-tool :bold :key/b)
               (rtetb/basic-formatting-tool :italic :key/i)
               (rtetb/basic-formatting-tool :underscore :key/u)
               (rtetb/basic-formatting-tool :strikethrough :key/s)
               (rtetb/block-formatting-tool :blockquote)
               (rtetb/block-formatting-tool :list :unordered-list)
               (rtetb/block-formatting-tool :list :ordered-list)
               (rtetb/link-tool)
               (rtetb/image-tool)
               (rtetb/video-tool)
               ]})

  (merge
    {:will-mount
     (fn [state]
       (let [{:keys [tools concat-tools]} (tools-ui/get-opts state)
             tools-nw (c/non-watchable state :tools)]
         (cond
           tools
           (c/rst! tools-nw tools)
           concat-tools
           (c/swp! tools-nw concat concat-tools)))
       state)}
    common/mix-opts-should-update)
  (rtef/opendoc-mixin)
  [state cursor opts]
  (let [{:keys [disabled? focused? key-down-spec]} opts
        container-class "rte-container"
        opendoc (c/actual (c/local state :opendoc))
        tools (c/actual (c/non-watchable state :tools))
        local (c/actual (c/local state :local))]
    [:div.full-editor
     {:class       (str container-class
                        (when (:focused? @local) " focus")
                        " " (get-in opts [:doc/conf :class]))
      :on-focus    (fn [_]
                     (c/swp! local assoc :focused? true))
      :on-blur     (fn [_] (c/swp! local dissoc :focused?))
      :on-key-down (fn [e]
                     (let [ev (uiev/wrap-event e :on-key-down)
                           kk (:kk ev)]
                       (when
                         (and
                           (or (:meta? ev) (:ctrl? ev))
                           (not ((uit/compose-set
                                   uit/clipboard-keys
                                   uit/history-keys)
                                  kk)))
                         (if-let [tool (rtetb/shortcut->tool @tools kk)]
                           (when-not disabled?
                             (rtetb/execute-tool state opendoc tool e))
                           (uiev/cancel-event ev)))))}
     (rtetb/rte-toolbox opendoc @tools
                        {:container-class container-class
                         :disabled?       disabled?})
     (rtef/rte-field opendoc
                     {:disabled?        disabled?
                      :focused?         focused?
                      :key-down-spec key-down-spec
                      :__opendoc-mixin/delayed (:__opendoc-mixin/delayed state)})
     (when (rtetb/active-tool-dialog? opendoc)
       (rtetb/rte-tool-dialog opendoc))]))

