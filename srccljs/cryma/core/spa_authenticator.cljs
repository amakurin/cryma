(ns cryma.core.spa-authenticator
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cryma.core.api :as api]
            [cljs.core.async :as async :refer [alts!]]
            [rum.core :as rum]
            [cryma.core.components.cursors :as c]
            [cryma.core.components.common :as common]
            [cryma.core.tools-ui :as tools-ui]))

(def reconnection-timeout-key :spa-authenticator/reconnection-timeout)
(def sign-out-uri-key :spa-authenticator/sign-out-uri)
(def state-key :spa-authenticator/state)
(def authenticated-predicate-key :spa-authenticator/unauthenticated-predicate)
(def username-render-fn-key :spa-authenticator/username-render-fn)
(def sign-up-event-key :spa-authenticator/sign-up-event)
(def sign-in-event-key :spa-authenticator/sign-in-event)
(def sign-out-event-key :spa-authenticator/sign-out-event)

(rum/defcs
  auth-panel
  < (c/cursored {:expanded false})
  (common/mix-global-click
    (fn [state e]
      (let [node (tools-ui/get-node state)
            inside (.contains node (.-target e))]
        (when-not inside
          (c/rst! (c/local state :expanded) false)))))
  [state spa-auth spa-auth-state-cursor]
  (let [l (:localizer spa-auth)
        username-render-fn (username-render-fn-key spa-auth)
        core (:core spa-auth)
        {:keys [principal status]} @spa-auth-state-cursor
        authenticated? (= :authenticated status)
        sign-up-event (sign-up-event-key spa-auth)
        sign-in-event (sign-in-event-key spa-auth)
        sign-out-event (sign-out-event-key spa-auth)
        expanded? (c/local state :expanded)]
    [:div.header-buttons
     {:class (when @expanded? "expanded")}
     [:ul.header-buttons
      (when-let [username (username-render-fn principal)]
        [:li.principal-info
         {:key "principal"}
         [:span.icon
          {:class (when-not authenticated? "guest")}]
         (if authenticated?
           (common/event-link spa-auth
                              {:qn :action.app/my-profile}
                              {}
                              [:span.text (if (string? username) username (l username))])
           [:span.text (if (string? username) username (l username))])])
      (when (and (not authenticated?) sign-up-event)
        [:li.head-btn.sign-up
         {:key      "signup"
          :on-click (fn [_]
                      (c/rst! expanded? false)
                      (api/pub-event core sign-up-event))}
         (l :spa-authenticator/sign-up)])
      (when (and (not authenticated?) sign-in-event)
        [:li.head-btn.sign-in
         {:key      "signin"
          :on-click (fn [_]
                      (c/rst! expanded? false)
                      (api/pub-event core sign-in-event))}
         (l :spa-authenticator/sign-in)])
      (when (and authenticated? sign-out-event)
        [:li.head-btn.sign-out
         {:key      "signout"
          :on-click (fn [_]
                      (c/rst! expanded? false)
                      (api/pub-event core sign-out-event))}
         (l :spa-authenticator/sign-out)])]
     [:div.sandwich
      {:on-click (fn [_] (c/swp! expanded? not))}]]))

(defn principal->state [spa-auth principal]
  (let [auth-pred (authenticated-predicate-key spa-auth)]
    {:principal principal
     :status    (if (auth-pred principal)
                  :authenticated :unauthenticated)}))

(defn handle-signed-in [spa-auth event]
  (let [core (:core spa-auth)
        reconnection-timeout (api/get-conf spa-auth reconnection-timeout-key)
        ch (async/chan 1)
        session (principal->state spa-auth (:principal event))]
    (api/write-val spa-auth nil session)
    (api/subscribe-event
      core
      {:msg-filter :transport/connection-opened
       :channel    ch})
    (println "Authentication:: subscribed to connection-opened")
    (api/pub-event core {:qn :transport/reconnect})
    (go
      (alts! [ch (async/timeout reconnection-timeout)])
      (async/close! ch)
      (if-let [redirect-ev (get-in event [:handled-event :redirect-event])]
        (api/pub-event core redirect-ev)
        (api/pub-event core {:qn :navigation/prev})))))

(defn handle-sign-out [spa-auth _]
  (api/write-val spa-auth nil (principal->state spa-auth nil))
  (.replace (.-location js/window) (api/get-conf spa-auth sign-out-uri-key)))

(defn handle-session-dropped [spa-auth event]
  (let [{:keys [spa localizer]} spa-auth]
    (api/show-message-box
      spa
      {:text (localizer (:qn event))
       :options
             [{:caption (localizer :spa-authenticator/reload-page)
               :event   {:qn :spa/sign-out}}]})))

(defn serve-authenticate [spa-auth request]
  (let [core (:core spa-auth)
        ch (api/call-async
             core (assoc request :target :server))]
    (async/pipe ch (api/get-response-chan core request))))

(defrecord SpaAuthenticator []
  api/IGenericKvStoreReader
  (read-val [reader korks]
    (get-in @(state-key reader) (if (vector? korks) korks [korks])))
  api/IGenericKvStoreManager
  (write-val [manager k data]
    (let [state (state-key manager)]
      (if (some? k)
        (swap! state assoc k data)
        (reset! state data))
      (api/pub-event (:core manager) {:qn :session/updated})))
  (update-val [manager korks data]
    (swap! (state-key manager) assoc-in korks data))
  (delete-val [manager k]
    (swap! (state-key manager) dissoc k))
  api/IModule
  api/IComponent
  (start [spa-auth]
    (let [spa (:spa spa-auth)
          state (atom (principal->state spa-auth (api/get-conf spa-auth :principal)))
          spa-auth (assoc spa-auth state-key state)]
      (api/mount-component spa :header
                           {:source-component spa-auth
                            :view-component   auth-panel
                            :view-args        [(c/cur state)]})
      spa-auth))
  (stop [spa-auth] spa-auth))

(defn new-spa-authenticator [& [m]]
  (map->SpaAuthenticator
    (merge
      {api/id-key                  :spa-authenticator
       api/static-deps-key         [:spa :localizer :uri-builder]
       api/request-servers-key     [{:msg-filter
                                              {:qn     :spa-authenticator/authenticate
                                               :target #{:client nil}}
                                     :handler serve-authenticate}]
       api/event-subs-key          [{:msg-filter :spa/signed-in
                                     :handler    handle-signed-in}
                                    {:msg-filter :spa/sign-out
                                     :handler    handle-sign-out}
                                    {:msg-filter :session/dropped
                                     :handler    handle-session-dropped}]
       api/configuration-key       {reconnection-timeout-key 5000
                                    sign-out-uri-key         "/sign-out/"}
       authenticated-predicate-key (fn [principal] (:id principal))
       username-render-fn-key      (fn [principal] (:username principal))
       sign-up-event-key           {:qn :spa/sign-up}
       sign-in-event-key           {:qn :spa/sign-in}
       sign-out-event-key          {:qn :spa/sign-out}}
      m)))