(ns cryma.core.components.rte.model.basic-formatting
  (:require
    [clojure.zip :as z]
    [cryma.core.components.rte.model.selection :as s]
    [cryma.core.components.rte.model.tools :as t]
    [cryma.core.components.rte.model.line-reduce :as lr]))

(defn apply-material-format
  [{:keys [format] :as material} {:keys [add-format remove-format]}]
  (let [add-formats (if (set? add-format) add-format (set add-format))
        remove-formats (if (set? remove-format) remove-format (set remove-format))
        original-formats (if (set? format) format (set format))
        clean-formats (remove remove-formats original-formats)
        result-formats (loop [add-formats add-formats res clean-formats]
                         (if-let [addf (first add-formats)]
                           (let [fk (if (keyword? addf) addf (first addf))
                                 res (->>
                                       res
                                       (remove (fn [format]
                                                 (if (keyword? format)
                                                   (= fk format)
                                                   (= fk (first format)))))
                                       (cons addf))]
                             (recur (rest add-formats) res))
                           res))]

    (assoc material :format (set result-formats))))

(defn process-basic-formatting
  [{:keys [doc selection] :as opendoc} ev]
  (let [{:keys [start end]} selection]
    (let [z (t/doc-zipper doc)
          start-line-pos (lr/z>line-position (t/zip-get-in z start))
          end-line-pos (lr/z>line-position (t/zip-get-in z end))
          z (t/zip-get-in z end)
          right-part (z/node (s/zip-delete-selection z {:end end}))
          z (z/replace z (z/node (s/zip-delete-selection z {:start end})))
          z (if right-part (z/insert-right z right-part) z)
          end-line-path (t/zip-short-path (z/up z))
          z (-> z z/root t/doc-zipper (t/zip-get-in start))
          right-part (z/node (s/zip-delete-selection z {:end start}))
          z (if-let [new (z/node (s/zip-delete-selection z {:start start}))]
              (z/insert-left z new) z)
          z (z/replace z right-part)
          start-line-path (t/zip-short-path (z/up z))
          new-end-path (-> z z/root t/doc-zipper
                           (t/zip-get-in end-line-path)
                           (lr/line-position>z end-line-pos)
                           t/zip-short-path)
          z (loop [z z]
              (let [z (if (t/zip-is-material? z)
                        (z/edit z apply-material-format ev)
                        z)]
                (if (= (t/zip-short-path z) new-end-path)
                  (-> z z/up lr/reduce-line)
                  (recur (z/next z)))))
          end (-> z
                  (lr/line-position>z end-line-pos)
                  t/zip-full-path)
          z (-> z z/root t/doc-zipper
                (t/zip-get-in start-line-path) lr/reduce-line)
          start (-> z
                    (lr/line-position>z start-line-pos)
                    t/zip-full-path)]
      (-> opendoc
          (assoc :doc (z/root z))
          (assoc :selection {:start start :end end})))))
