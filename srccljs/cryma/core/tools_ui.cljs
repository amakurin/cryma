(ns cryma.core.tools-ui
  (:require
    [cryma.core.tools :as tools]
    [rum.core :as rum]
    [cryma.core.api :as api]))

(def css-trans-group (-> js/React (aget "addons") (aget "CSSTransitionGroup")))

(defn wrap-css-transit [show-component? component cparams tparams-or-name]
  (let [p (if (string? tparams-or-name)
            #js {:transitionEnterTimeout 0
                 :transitionLeaveTimeout 300
                 :component              "div"
                 :transitionName         tparams-or-name}
            tparams-or-name)]
    (.createElement
      js/React css-trans-group p
      (when show-component?
        (apply component cparams)))))


(defn wrap-center [view & [{:keys [class]}]]
  [:div.center-wrapper
   {:class class}
   [:div.center view]])

(defn event->route [module event]
  (if-let [uri-builder (:uri-builder module)]
    (uri-builder event)
    (throw (ex-info "Module missing :uri-builder"
                    {:module-id (api/id-key module)
                     :event-qn  (:qn event)}))))

(defn get-comp [state]
  (:rum/react-component state))

(defn get-node [state & [ref]]
  (let [comp (get-comp state)]
    (js/ReactDOM.findDOMNode
      (if ref
        (aget (.-refs comp) ref)
        comp))))

(defn get-opts [state & korks]
  (tools/get-korks
    (-> state :rum/args last)
    korks))

