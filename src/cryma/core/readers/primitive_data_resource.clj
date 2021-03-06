(ns cryma.core.readers.primitive-data-resource
  (:require [cryma.core.api :as api]))

(defrecord Reader []
  api/IGenericResourceReader
  (read-resource [reader] (:datareader/data reader)))

(defn new-reader [data & [m]]
  (map->Reader (assoc m :datareader/data data)))
