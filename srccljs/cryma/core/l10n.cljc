(ns cryma.core.l10n
  (:require
    [cryma.core.dtl.core :as dtl]
    [cryma.core.dtl.transformers.str :as str]
    [cryma.core.dtl.transformers.mapping :as mapping]
    [clojure.string :as clostr]
    ))

(defmulti
  plural-form
  (fn [locale-id n]
    {:pre [(number? n)]}
    (when locale-id (clostr/lower-case (name locale-id)))))

(defmethod plural-form "ru"
  [_ n]
  (let [d00 (quot (rem n 100) 10)
        d0 (rem n 10)]
    (if (= 1 d00)
      5
      (cond
        (or (= d0 0) (>= d0 5)) 5
        (>= d0 2) 2
        :else 1))))

(defmethod plural-form :default
  [_ n]
  (if (or (= n 0) (> n 1)) 2 1))

(defn pluralize [voc q plural]
  (case plural
    1 (get voc q (get voc [q 1]))
    2 (get voc [q 2] (pluralize voc q 1))
    5 (get voc [q 5] (get voc q (pluralize voc q 2)))))

(defn localize
  ([locale q] (localize locale q 1))
  ([locale q n]
   (let [{:keys [voc locale-id]} locale]
     (if (nil? q)
       ""
       (if-not voc
         (pr-str q)
         (let [plural (plural-form locale-id n)
               voc-k (if (map? q) (:code q) q)
               translation-rule (pluralize voc voc-k plural)]
           (cond
             (string? translation-rule) translation-rule
             (and (vector? translation-rule) (map? q))
             (dtl/transform
               q translation-rule
               {:default str/transformer
                :mapping mapping/transformer}
               (fn [context using-value]
                 (let [{:keys [transk custom]} context]
                   (cond
                     (= :plurality custom) (plural-form locale-id using-value)
                     (= :mapping transk) using-value
                     :else
                     (if (or (map? using-value)
                             (keyword? using-value)
                             (vector? using-value))
                       (localize locale using-value n) using-value)))))
             (and (nil? translation-rule) (keyword? q)) (name q)
             (and (nil? translation-rule) (map? q) (:code q)) (name (:code q))
             :else (str q))))))))


