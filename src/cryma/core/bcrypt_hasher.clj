(ns cryma.core.bcrypt-hasher
  (:require [cryma.core.api :as api])
  (:import
    [org.apache.commons.codec.binary Base64]
    [javax.crypto Mac]
    [javax.crypto.spec SecretKeySpec]
    [org.mindrot.jbcrypt BCrypt]))

(defn compute-hmac [msg k]
  (let [mac-alg "HmacSHA256"
        spec (SecretKeySpec. (.getBytes k "UTF-8") mac-alg)
        mac (Mac/getInstance mac-alg)]
    (.init mac spec)
    (Base64/encodeBase64String (.doFinal mac (.getBytes msg "UTF-8")))))

(defrecord Hasher []
  api/IHasher
  (hash-value
    [hasher str-value]
    (api/hash-value hasher str-value nil))
  (hash-value
    [hasher str-value work-factor]
    (BCrypt/hashpw
      (str str-value
           (compute-hmac str-value (api/get-conf hasher :secret-key)))
      (if work-factor
        (BCrypt/gensalt work-factor)
        (BCrypt/gensalt))))
  (verify-hash
    [hasher str-value str-hash]
    (BCrypt/checkpw
      (str str-value
           (compute-hmac str-value (api/get-conf hasher :secret-key))) str-hash))
  api/IComponent
  (start [hasher] hasher)
  (stop [hasher] hasher))

(defn new-hasher [secret-key & [m]]
  (map->Hasher
    (merge
      {api/id-key            :hasher
       api/configuration-key {:secret-key secret-key}}
      m)))