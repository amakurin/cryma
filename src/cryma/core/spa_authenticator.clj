(ns cryma.core.spa-authenticator
  (:require [cryma.core.api :as api]
            [clojure.core.async :as async]))

(def request->session-id-fn-key :spa-authenticator/session-id-fn)
(def principal->user-id-fn :spa-authenticator/user-id-fn)
(def signout-redirect-route-key :spa-authenticator/signout-redirect-route)
(def sign-out-uri-key :spa-authenticator/sign-out-uri)
(def guest-user-key :spa-authenticator/guest-user)

(defn serve-authenticate [spa-auth request]
  (let [core (:core spa-auth)
        session-writer (:session-manager spa-auth)
        credentials (:credentials request)
        response (async/<!! (api/call-async
                              core
                              {:qn          :user-directory/authenticate-user
                               :credentials credentials}))]
    (when (api/success? core response)
      (when-let [session-id ((request->session-id-fn-key spa-auth) request)]
        (let [principal (:result response)]
          (api/update-val session-writer
                          [session-id :user-id]
                          ((principal->user-id-fn spa-auth) principal)))))
    (api/respond core request response)))

(defn unauthenticate [spa-auth request]
  (let [session-writer (:session-manager spa-auth)]
    (when-let [session-id ((request->session-id-fn-key spa-auth) request)]
      (api/update-val session-writer [session-id :user-id] nil))))

(defn serve-unauthenticate [spa-auth request]
  (let [core (:core spa-auth)]
    (unauthenticate spa-auth request)
    (api/respond-success core request)))

(defn sign-out-handler [spa-auth ring-request]
  {:status  302
   :headers {"Location" (api/get-conf spa-auth signout-redirect-route-key)}
   :session (dissoc (:session ring-request) :user-id)
   :body    ""})

(defrecord SpaAuthenticator []
  api/IClientStateProvider
  (provide-client-state [spa-auth ring-request]
    (let [core (:core spa-auth)
          user-id (or (get-in ring-request [:session :user-id])
                      (guest-user-key spa-auth))
          response (async/<!! (api/call-async
                                core
                                {:qn      :user-directory/get-user-info
                                 :user-id user-id}))]
      (if (api/success? core response)
        {:principal       (:result response)}
        (println "ERROR:" response))))
  api/IModule
  api/IComponent
  (start [spa-auth] spa-auth)
  (stop [spa-auth] spa-auth))

(defn new-spa-authenticator [& [m]]
  (map->SpaAuthenticator
    (merge
      {api/id-key                 :spa-authenticator
       api/static-deps-key        [:session-manager]
       api/request-servers-key    [{:msg-filter :spa-authenticator/authenticate
                                    :handler    serve-authenticate}
                                   {:msg-filter :spa-authenticator/unauthenticate
                                    :handler    serve-unauthenticate}]
       api/routes-key             [["/sign-out/" :get {:handler sign-out-handler}]]
       request->session-id-fn-key (fn [request] (get-in (meta request)
                                                        [:ring-req :session/key]))
       principal->user-id-fn      (fn [principal] (:id principal))
       api/configuration-key      {signout-redirect-route-key "/"}
       guest-user-key :guest}
      m)))

