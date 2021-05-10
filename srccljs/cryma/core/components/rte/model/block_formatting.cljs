(ns cryma.core.components.rte.model.block-formatting
  (:require
    [clojure.zip :as z]
    [cryma.core.components.rte.model.tools :as t]))

(defn ->if [arg arg-pred-or-bool f & args]
  (if (or (and (fn? arg-pred-or-bool) (arg-pred-or-bool arg))
          (true? arg-pred-or-bool))
    (apply f arg args)
    arg))


(defn -copy-line-selection
  [content {:keys [start end]} {:keys [omit-start? omit-end?] :as omit}]
  (let [start-pos (first start)
        end-pos (first end)]
    (->>
      content
      (map-indexed
        (fn [index item]
          (let [item-is-block? (= :block (:type item))
                item-is-line? (= :line (:type item))]
            (cond
              (and item-is-block? (= index start-pos end-pos))
              (update item :content -copy-line-selection
                      {:start (rest start) :end (rest end)} omit)
              (and item-is-block? (= index start-pos))
              (update item :content -copy-line-selection
                      {:start (rest start)} omit)
              (and item-is-block? (= index end-pos))
              (update item :content -copy-line-selection
                      {:end (rest end)} omit)
              (or (and item-is-line? (= index start-pos) (not omit-start?))
                  (and item-is-line? (= index end-pos) (not omit-end?)))
              item
              (and (or (nil? start-pos) (> index start-pos))
                   (or (nil? end-pos) (< index end-pos)))
              item))))
      (remove nil?)
      (remove (fn [{:keys [type content]}]
                (and (= :block type) (empty? content))))
      vec)))

(defn copy-line-selection
  [content {:keys [start end] :as selection}]
  (-copy-line-selection content selection {:omit-start? (nil? end)
                                           :omit-end?   (nil? start)}))

(defn zip-add-block-format [z {:keys [start end] :as selection} add-format]
  (-> z
      (z/edit
        update :content
        (fn [content]
          (let [first-part (copy-line-selection content {:end start})
                to-modify (copy-line-selection content selection)
                last-part (copy-line-selection content {:start end})]
            (t/concatv
              first-part
              (if (and (= 1 (count to-modify))
                       (-> to-modify first :type #{:block}))
                (update to-modify 0 merge
                        (select-keys add-format [:semantics :format]))
                (->
                  add-format
                  (assoc :type :block)
                  (assoc :content
                         (->> to-modify
                              (mapcat (fn [item]
                                        (if (= :line (:type item))
                                          [item] (:content item))))
                              vec))
                  vector))
              last-part))))))

(defn flatten-lines [content]
  (->> content
       (mapcat (fn [item]
                 (if (= :line (:type item))
                   [item]
                   (flatten-lines (:content item)))))
       vec))

(defn zip-remove-block-format [z {:keys [start end] :as selection} _]
  (-> z
      (z/edit
        update :content
        (fn [content]
          (let [first-part (copy-line-selection content {:end start})
                to-modify (copy-line-selection content selection)
                last-part (copy-line-selection content {:start end})]
            (t/concatv
              first-part
              (flatten-lines to-modify)
              last-part))))))

(defn assoc-meta [o & args]
  (with-meta o (apply assoc (meta o) args)))

(defn add-markers [z {:keys [start end]}]
  (->
    z
    (t/zip-get-in start)
    (z/edit assoc-meta :doc.model.markers/start (peek start))
    z/root t/doc-zipper
    (t/zip-get-in end)
    (z/edit assoc-meta :doc.model.markers/end (peek end))
    z/root t/doc-zipper))

(defn remove-markers [z]
  (loop [z (z/next z) selection {:start nil :end nil}]
    (let [node (when z (z/node z))
          start-marker (-> node meta :doc.model.markers/start)
          end-marker (-> node meta :doc.model.markers/end)
          new-selection
          (->
            selection
            (->if (not (nil? start-marker))
                  assoc :start (conj (t/zip-short-path z) start-marker))
            (->if (not (nil? end-marker))
                  assoc :end (conj (t/zip-short-path z) end-marker)))]
      (if (or end-marker (t/zip-root? z))
        {:z (-> z (z/edit with-meta {}) z/root t/doc-zipper) :selection new-selection}
        (recur (-> z
                   (z/edit with-meta {})
                   z/next) new-selection)))))

(defn process-block-formatting
  [{:keys [doc selection] :as opendoc} {:keys [add-format remove-format]}]
  (let [z (add-markers (t/doc-zipper doc) selection)
        z (if add-format
            (zip-add-block-format z selection add-format)
            (zip-remove-block-format z selection remove-format))
        {:keys [z selection]} (remove-markers z)]

    (-> opendoc
        (assoc :doc (z/root z))
        (assoc :selection selection))))
