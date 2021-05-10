(ns cryma.core.default-auth-screen
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require
    [cljs.core.async :refer [<!]]
    [rum.core :as rum]
    [cryma.core.schema :as sch :refer-macros [comp-schema]]
    [cryma.core.components.cursors :as c]
    [cryma.core.api :as api]
    [cryma.core.tools-ui :as tools-ui]
    [cryma.core.components.common :as common]
    [cryma.core.components.channels :as ch]
    [cryma.core.components.input :as input]))

(def auth-schema
  (comp-schema
    sch/required-constraint
    sch/data-type-map
    [:map/spec-closed
     {:login (comp-schema
               [:data/type :data-type/string]
               [:string/not-empty])
      :pwd   (comp-schema
               [:data/type :data-type/string]
               [:string/not-empty])}]))

(defn try-sign-in [module component-state data]
  (let [core (:core module)
        credentials (select-keys @component-state [:login :pwd])
        errors (seq (sch/errors? auth-schema credentials))
        errors (when errors {:status :error
                             :errors (sch/flatten-map-errors errors)})]
    (c/swp! component-state
            assoc
            :status (or errors {:status      :processing
                                :description :status/checking-credentials}))
    (when-not errors
      (go
        (let [response
              (<! (api/call-async
                    core
                    {:qn          :spa-authenticator/authenticate
                     :credentials credentials}))]
          (c/swp! component-state assoc :status response)
          (when (api/success? core response)
            (api/pub-event
              core
              {:qn            :spa/signed-in
               :principal     (:result response)
               :handled-event @data})))))))

(defn render-sign-in-status [module status]
  (when status
    (let [l (:localizer module)]
      [:div.sign-in-status
       {:class (when (= :error (:status status)) "error")}
       [:ul
        (case (:status status)
          :error (map (fn [e] [:li (l e)]) (:errors status))
          :processing [:li (if-let [descr (:description status)]
                             (l descr) (l :processing))]
          :success [:li (l :transport/reconnecting)])]])))

(defn error-field? [field errors]
  (->> errors
       (filter #(= field (:field %)))
       first))

(rum/defcs
  user-sign-in
  < (c/cursored {:local {:login "" :pwd "" :disabled? false :status nil}})
    (ch/channels)
    (c/cursored-chans
      (c/local-path :local)
      (fn [state ev]
        (when (= (:qn ev) :on-key-down)
          (let [module (first (:rum/args state))
                data (second (:rum/args state))
                local (c/actual (c/local state :local))]
            (case (:kk ev)
              :key/enter
              (let [field (last (:data-path ev))]
                (case field
                  :login
                  (when-let [node (.getElementById js/document "pwd-field")]
                    (.focus node))
                  :pwd
                  (when-not (:disabled? @local)
                    (try-sign-in module local data))
                  nil))
              nil)))))
    {:will-mount
     (fn [state]
       (let [module (first (:rum/args state))
             {:keys [core session-reader]} module
             auth? (= :authenticated (api/read-val session-reader [:status]))]
         (when auth?
           (api/pub-event core {:qn :action.app/browse-posts})))
       state)}
  [state module data]
  (let [{:keys [session-reader localizer]} module
        local (c/actual (c/local state :local))
        auth? (= :authenticated (api/read-val session-reader [:status]))
        {:keys [disabled? status]} @local]
    (when (not auth?)
      (tools-ui/wrap-center
        [:div
         [:div.user-sign-in
          [:ul.form
           (common/wrap-frow
             module :login local
             (input/cursored-input
               module (:login local)
               {:placeholder (localizer :sign-in-login-placeholder)
                :id          "login-field"
                :disabled    disabled?
                :maxLength   20}))
           (common/wrap-frow
             module :pwd local
             (input/cursored-input
               module (:pwd local)
               {:placeholder (localizer :sign-in-pwd-placeholder)
                :id          "pwd-field"
                :type        "password"
                :disabled    disabled?
                :maxLength   20}))
           [:li.frow
            [:span.btn
             {:class    (when disabled? "disabled")
              :on-click (fn [_] (when-not disabled? (try-sign-in module local data)))}
             (localizer :sign-in-button)]]]]
         (render-sign-in-status module status)]))))

(defrecord AuthenticationScreen []
  api/IModule
  api/IComponent
  (start [comp] comp)
  (stop [comp] comp))

(defn new-authentication-screen [& [m]]
  (map->AuthenticationScreen
    (merge
      {api/id-key          :authentication-screen
       api/static-deps-key [:localizer :core :session-reader]
       api/routes-key      [{:msg-filter      :spa/sign-in
                             :route-component user-sign-in}]}
      m)))

