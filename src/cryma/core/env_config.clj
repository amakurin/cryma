(ns cryma.core.env-config
  (:require
    [clojure.tools.reader.edn :as edn]
    [environ.core :refer [env]]
    [cryma.core.tools :as tools]))

(defn read-config-val [v]
  (if (string? v)
    (let [as-is "as-is::"]
      (if (.startsWith v as-is)
        (clojure.string/replace v (re-pattern as-is) "")
        (try
          (edn/read-string v)
          (catch Exception e v))))
    v))


(defn conf [korks & [default]]
  (let [path (tools/korks-path korks)
        k (first path)
        root-v (read-config-val (env k))]
    (if root-v
      (if-let [path (seq (rest path))]
        (when (map? root-v)
          (get-in root-v path default))
        root-v)
      default)))