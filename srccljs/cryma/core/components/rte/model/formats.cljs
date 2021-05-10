(ns cryma.core.components.rte.model.formats
  (:require
    [clojure.string :as clostr]
    [clojure.set :as closet]))

(defn format->k [format]
  (if (vector? format) (first format) format))

(defn get-format-renderer [conf format]
  (get-in conf [:formats (format->k format) :render-format-fn]))

(defn render-format [conf format]
  (if-let [format-renderer (get-format-renderer conf format)]
    (format-renderer format)
    [:class (name (format->k format))]))

(defmulti consolidate-attr (fn [k _] k))

(defmethod consolidate-attr :class
  [_ classes]
  (clostr/join " " classes))

(defmethod consolidate-attr :style
  [_ styles]
  (clostr/join "" styles))

(defn render-formats [formats conf]
  (->>
    formats
    (map (partial render-format conf))
    (group-by first)
    (map (fn [[k v]] [k (consolidate-attr k (map peek v))]))
    (into {})))

(defn intersect-formats [fs1 fs2]
  (->> (closet/intersection
         (set (map format->k fs1)) (set (map format->k fs2)))
       (map (fn [k] (some #(and (= (format->k %) k) %) fs1)))
       set))

#_(intersect-formats #{:bold [:font "arial"] :italic}
                     #{:bold [:font "curier"] :underscore})
