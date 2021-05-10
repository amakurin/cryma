(ns cryma.core.components.rte.model.convertation
  (:require
    [clojure.zip :as z]
    [clojure.string :as clostr]
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [cryma.core.components.rte.model.tools :as t]
    [hickory.render :as hr]
    [hickory.core :as hp]
    [hickory.zip :as hz]
    [cryma.core.components.rte.model.configuration :as conf]
    [cryma.core.components.rte.model.semantics.protocols :as protocols]
    [cryma.core.components.rte.model.formats :as formats]
    [cljs.reader :as r]))

;;=============================
;; EXPORT/IMPORT
;;=============================

(defn zip-generic-traversal [dz visitor]
  (cond
    (nil? dz) nil
    (z/branch? dz)
    (visitor dz
             (loop [z (z/down dz) res []]
               (let [res (conj res (zip-generic-traversal z visitor))]
                 (if (t/zip-rightmost? z)
                   res
                   (recur (z/right z) res)))))
    :else
    (visitor dz)))

;;=============================
;; TEXT
;;=============================

(defmulti -export-text t/zip-type-dispatch)

(defn export-text [dz & [text-only?]]
  (zip-generic-traversal
    dz (if text-only?
         (fn [z ch]
           (let [node (z/node z)]
             (if (= :material (:type node))
               (if (string? (:content node))
                 (-export-text z ch)
                 " ")
               (-export-text z ch))))
         -export-text)))

(defmethod -export-text :material
  [z _]
  (let [conf (conf/zip-get-conf z)
        logic (conf/get-semantics conf (t/zip-semantics z) :logic)]
    (protocols/to-text logic (z/node z) conf)))

(defmethod -export-text :line
  [_ children]
  (clostr/join children))

(defmethod -export-text :block
  [_ children]
  (clostr/join "\n" children))

(defn import-text [text-plain]
  {:type      :block
   :semantics :document
   :content   (->>
                text-plain
                clostr/split-lines
                (mapv (fn [line]
                        {:type    :line
                         :content [{:type      :material
                                    :semantics :text
                                    :content   line}]})))})

(defn get-text [z]
  (->
    z
    (export-text :text-only!)
    (clostr/replace #"\n" " ")
    clostr/trim))

;;=============================
;; HTML
;;=============================

(defn item-attrs [item]
  {:data-keks (pr-str (t/significant-params (z/node item)))})

(defmulti -export-html t/zip-type-dispatch)

(defn zip-export-html [dz]
  (->
    dz
    (zip-generic-traversal -export-html)
    hr/hickory-to-html))

(defn export-html [doc]
  (->
    doc
    t/doc-zipper
    zip-export-html))

(defmethod -export-html :material
  [z _]
  (let [conf (conf/zip-get-conf z)
        logic (conf/get-semantics conf (t/zip-semantics z) :logic)
        node (z/node z)]
    {:type    :element
     :attrs   (merge (item-attrs z)
                     (protocols/html-attrs logic node conf)
                     (formats/render-formats (:format node) conf))
     :tag     (protocols/html-tag logic node conf)
     :content (protocols/html-content logic node conf)}))

(defmethod -export-html :line
  [z children]
  (let [conf (conf/zip-get-conf z)
        block-logic (conf/get-semantics conf (t/zip-semantics (z/up z)) :logic)
        node (z/node z)
        empty-line? (= node (t/create-empty-line))]
    {:type    :element
     :attrs   (merge (item-attrs z)
                     (formats/render-formats (:format node) conf))
     :tag     (or (when block-logic (protocols/html-line-tag block-logic node conf))
                  (if empty-line? :br :p))
     :content (if (or block-logic (not empty-line?)) children [])}))

(defmulti -block-semantic-export-html t/zip-semantic-dispatch)

(defmethod -block-semantic-export-html :document
  [z children]
  (let [attrs (item-attrs z)]
    {:type    :document
     :attrs   attrs
     :content [{:type    :element
                :tag     :html
                :content [{:type    :element
                           :tag     :head
                           :content []}
                          {:type    :element
                           :tag     :body
                           :attrs   attrs
                           :content children}]}]}))

(defmethod -block-semantic-export-html :default
  [z children]
  (let [conf (conf/zip-get-conf z)
        logic (conf/get-semantics conf (t/zip-semantics z) :logic)
        node (z/node z)]
    {:type    :element
     :attrs   (merge (item-attrs z)
                     (when logic (protocols/html-attrs logic node conf))
                     (formats/render-formats (:format node) conf))
     :tag     (or (when logic (protocols/html-tag logic node conf)) :p)
     :content children}))

(defmethod -export-html :block
  [z children]
  (-block-semantic-export-html z children))

(defn go-to-body [hz]
  (loop [z hz]
    (if (-> z z/node :tag #{:body})
      (hz/hickory-zip (z/node z))
      (recur (z/next z)))))

(defn create-doc-node [z children]
  (let [node (z/node z)]
    (if-let [keks (-> node :attrs :data-keks)]
      (let [keks (r/read-string keks)]
        (-> keks
            (assoc :content
                   (cond
                     (= :material (:type keks))
                     (clostr/join children)
                     (and (= :line (:type keks))
                          (or (= children [nil]) (empty? children)))
                     [(t/create-empty-text-material)]
                     :else
                     children))))
      node)))

(defn import-html [text-html]
  (let [hz (->
             text-html
             hp/parse
             hp/as-hickory
             hz/hickory-zip
             go-to-body)
        attrs (:attrs (z/node hz))]
    (when (and attrs (-contains-key? attrs :data-keks))
      (zip-generic-traversal hz create-doc-node))))

(defn import-html-or-empty [text-html]
  (if-let [doc (import-html text-html)]
    doc
    (t/create-empty-document)))

