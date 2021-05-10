(ns cryma.core.components.rte.model.line-reduce
  (:require [clojure.zip :as z]
            [cryma.core.components.rte.model.tools :as t]
            [cryma.core.components.rte.model.selection :as s]
            [cryma.core.components.rte.model.configuration :as configuration]))

(defn z>line-position [mat-z]
  (let [line-pos (t/zip-branch-pos mat-z)
        mat-pos (peek (t/zip-full-path mat-z))]
    (loop [z (z/leftmost mat-z) sum 0 i 0]
      (if (< i line-pos)
        (recur (z/right z) (+ sum (t/zip-count-inner-positions z)) (inc i))
        (+ sum mat-pos)))))

(defn line-position>z [line-z line-pos]
  (loop [z (z/down line-z) sum line-pos]
    (let [new-sum (- sum (t/zip-count-inner-positions z))]
      (cond
        (<= new-sum 0)
        (t/zip-get-in z [sum])
        (t/zip-rightmost? z) nil
        :else (recur (z/right z) new-sum)))))

(defn reduce-line [line-z]
  (loop [z (z/down line-z)]
    (let [empty-z? (t/-empty? z)
          rightmost-z? (t/zip-rightmost? z)
          leftmost-z? (t/zip-leftmost? z)]
      (cond

        (and empty-z? rightmost-z? leftmost-z?)
        (z/up z)

        (and empty-z? leftmost-z?)
        (recur (-> z z/remove z/next))

        empty-z?
        (recur (z/remove z))

        (and (not leftmost-z?)
             (t/same-params? (z/node (z/left z)) (z/node z)))
        (let [joined (s/-join-nodes (z/node (z/left z)) (z/node z)
                                    (configuration/zip-get-conf z))]
          (if (next joined)
            (if rightmost-z? (z/up z) (recur (z/right z)))
            (recur (-> z z/remove (z/replace (peek joined))))))

        rightmost-z?
        (z/up z)

        :else
        (recur (z/right z))))))

(defn reduce-lines [z]
  (case (-> z z/node :type)
    :line (reduce-line z)
    :block (loop [z (z/down z)]
             (let [z (reduce-lines z)]
               (if (t/zip-rightmost? z)
                 (z/up z)
                 (recur (z/right z)))))
    z))

(defn reduce-parent-line [mat-z]
  (let [line (z/up mat-z)]
    (cond
      (t/zip-is-material? mat-z)
      (let [line-pos (z>line-position mat-z)
            reduced-line (reduce-line line)]
        (line-position>z reduced-line line-pos))
      (and (t/zip-is-type? line :line) (nil? (z/node mat-z)))
      (z/replace mat-z (t/set-material-position (t/create-empty-text-material) 0))
      :else
      mat-z)))

