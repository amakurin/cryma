(ns cryma.core.components.rte.model.clipboard
  (:require
    [clojure.zip :as z]
    [cryma.core.components.rte.model.selection :as s]
    [cryma.core.components.rte.model.tools :as t]
    [cryma.core.components.rte.model.convertation :as c]))

;;=============================
;; PASTE
;;=============================

;; this one allows nesting blocks
#_(defn concat-content [z item]
    (z/edit
      z (fn [node]
          (update
            node :content
            (fn [content]
              (t/concatv content
                         (:content item)))))))

(defn get-lines [item]
  (let [block? (= :block (:type item))]
    (if block?
      (vec (mapcat get-lines (:content item)))
      [item])))

;; and this does not
(defn concat-content [z item]
  (z/edit
    z (fn [node]
        (update
          node :content
          (fn [content]
            (t/concatv
              content
              (cond
                (and (= :block (:type node))
                     (not= :document (:semantics node))
                     (= :block (:type item)))
                (get-lines item)
                (and (= :block (:type node))
                     (= :block (:type item))
                     (not= :document (:semantics item)))
                [item]
                :else
                (:content item))))))))

(defn join-next [z full-path]
  (let [rootz (t/doc-zipper (z/root z))
        next-path (s/zip-closest-material-next-path
                    (t/zip-get-in rootz full-path))]
    (s/zip-delete-selection
      z {:start full-path
         :end   next-path})))

(defn paste-fragment [dz zfrag {:keys [start]}]
  (let [part-before-frag (s/zip-delete-selection dz {:start start})
        part-after-frag (s/zip-copy-selection dz {:start start})
        z (z/edit dz (fn [_] (z/node part-before-frag)))
        path-before-frag (t/zip-end-path z)
        z (concat-content z (z/node zfrag))
        z (join-next z path-before-frag)
        path-after-frag (t/zip-end-path z)
        z (concat-content z (z/node part-after-frag))
        z (join-next z path-after-frag)]
    (t/zip-get-in (t/doc-zipper (z/root z)) path-after-frag)))

(defn first-parent-block [dz]
  (loop [z dz]
    (cond
      (= :block (:type (z/node z)))
      z
      (not (t/zip-root? z))
      (recur (z/up z)))))

(defn zip-paste [dz zfragment range]
  (let [src-block (first-parent-block dz)]
    (if (and (z/branch? zfragment) (> (count (z/children zfragment)) 1))
      (paste-fragment src-block zfragment range)
      (let [zfragment (t/doc-zipper (z/node (z/next zfragment)))
            fragment-type (:type (z/node zfragment))]
        (loop [z dz]
          (cond
            (= (:type (z/node z)) fragment-type)
            (paste-fragment z zfragment range)
            (not (t/zip-root? z))
            (recur (z/up z))))))))

(defn parse-clipboard [clipboard]
  (let [settings {:text/plain {:importer c/import-text :priority 0}
                  :text/html  {:importer c/import-html :priority 1}}]
    (->>
      clipboard
      (map
        (fn [[k v]]
          (when-let [{:keys [importer priority]} (k settings)]
            [v importer priority])))
      (remove nil?)
      (sort-by peek >)
      ((fn [clipboards]
         (loop [clipboards clipboards]
           (when-let [[v importer _] (first clipboards)]
             (if-let [fragment (importer v)]
               fragment
               (recur (rest clipboards))))))))))

(defn process-paste [z range {:keys [clipboard]}]
  (if-let [fragment (parse-clipboard clipboard)]
    (zip-paste z (t/doc-zipper fragment) range)
    z))

;;=============================
;; COPY
;;=============================

(defn process-copy [z range _]
  (when-not (s/selection-collapsed? range)
    (let [fragment (s/zip-copy-selection-minimized
                     z range)]
      {:text/plain (c/export-text fragment)
       :text/html  (c/zip-export-html fragment)})))