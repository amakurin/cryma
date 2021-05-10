(ns cryma.app.qr-service
  (:require [clj.qrgen :as qr]
            [cryma.core.api :as api]
            [ring.util.response :refer (content-type)]))

(defrecord QrService []
  api/IModule
  api/IComponent
  (start [qr-service]
    qr-service)
  (stop [qr-service]
    qr-service))

(defn parse-size [size-str & [default]]
  (try
    (Integer. size-str)
    (catch Exception _ (or default 128))))

(defn handle-qr-service [qr-service request]
  (let [{:keys [address amount size]
         :or   {address "WTF?!" amount "0" size "128"}} (:params request)
        size (parse-size size)]
    (->
      {:status 200
       :body   (qr/as-input-stream
                 (qr/from (str "bitcoin:" address "?amount=" amount)
                          :correction qr/L
                          :size [size size]
                          :image-type qr/JPG))}
      (content-type "image/jpeg"))))

(defn new-qr-service [& [m]]
  (map->QrService
    (merge
      {api/id-key :qr-service
       api/routes-key
                  [["/qr-service/" :get [:address "/" :amount "/"]
                    {:handler handle-qr-service}]]}
      m)))

