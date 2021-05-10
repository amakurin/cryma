(ns cryma.core.components.rte.model.configuration
  (:require [clojure.zip :as z]))

;;=============================
;; CONFIGURATION
;;=============================

(defn merge-configs [res mrg]
  (cond
    (and (map? res) (map? mrg))
    (merge-with merge-configs res mrg)
    (nil? mrg) res
    :else mrg))

(defn create-configuration [default & merge-confs]
  (apply merge-with
         merge-configs
         default
         merge-confs))

(defn with-conf [doc conf]
  (with-meta doc (update (or (meta doc) {}) :conf merge-configs conf)))

(defn get-conf [doc]
  (-> doc meta :conf))

(defn zip-get-conf [z]
  (when z (-> z z/root get-conf)))

(defn get-semantics [conf semantics & ks]
  (get-in conf (concat [:semantics semantics] ks)))

(defn zip-get-semantics [z & ks]
  (if-let [conf (zip-get-conf z)]
    (apply get-semantics conf (-> z z/node :semantics) ks)
    (throw (ex-info "Configuration was not found" {:z z}))))