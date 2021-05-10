(ns cryma.core.localizer
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require
    [cljs.core.async :as async :refer [<!]]
    [cryma.core.api :as api]
    [cryma.core.l10n :as l10n]
    ))

(defrecord Localizer []
  IFn
  (-invoke [localizer q]
    (localizer q 1))
  (-invoke [localizer q n]
    (let [locale (api/get-conf localizer :current-locale)]
      (l10n/localize @locale q n)))
  api/IModule
  api/IComponent
  (start [localizer]
    (let [locale (api/get-conf localizer :current-locale)]
      (assoc-in localizer
                [api/configuration-key :current-locale] (atom locale))))
  (stop [localizer] localizer))

(defn route-to-server [core request]
  (let [ch (api/call-async
             core (assoc request :target :server))]
    (async/pipe ch (api/get-response-chan core request))))

(defn serve-get-supported-locales [localizer request]
  (route-to-server (:core localizer) request))

(defn serve-get-locale [localizer request]
  (route-to-server (:core localizer) request))

(defn serve-set-locale [localizer request]
  (let [core (:core localizer)
        ch (api/call-async
             core
             {:qn        :localizer/get-locale
              :locale-id (:locale-id request)})]
    (go
      (let [response (<! ch)]
        (if (api/success? core response)
          (let [locale (:result response)]
            (reset! (api/get-conf localizer :current-locale) locale)
            (api/respond-success core request))
          (api/respond core request response))))))

(defn new-localizer [& [m]]
  (map->Localizer
    (merge
      {api/id-key :localizer
       api/request-servers-key
                  [{:msg-filter {:qn :localizer/get-supported-locales
                                 :target #{:client nil}}
                    :handler    serve-get-supported-locales}
                   {:msg-filter {:qn :localizer/get-locale
                                 :target #{:client nil}}
                    :handler    serve-get-locale}
                   {:msg-filter :localizer/set-locale
                    :handler    serve-set-locale}]}
      m)))
