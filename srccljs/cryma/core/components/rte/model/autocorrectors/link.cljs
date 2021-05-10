(ns cryma.core.components.rte.model.autocorrectors.link
  (:require
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [cryma.core.components.rte.model.autocorrectors.protocols :as protocols]
    [cryma.core.components.rte.model.tools :as t]
    [cryma.core.components.rte.model.selection :as s]
    [clojure.string :as clostr]
    [clojure.zip :as z]))

(defn positions-to-split [s]
  (->>
    s
    (re-find #"(?im)\b(https?://|ftp://|www\.)[^\s]+")
    ((fn [found]
       (when-let [url (first found)]
         (let [start-pos (clostr/index-of s url)
               url (clostr/lower-case url)]
           [start-pos (+ start-pos (count url))
            (if (clostr/starts-with? url "http") url (str "http://" url))]))))))

(defn link-autocorrector []
  (reify
    protocols/IAutoCorrector
    (applicable? [_ opendoc {:keys [qn] :as ev}]
      (and
        (s/selection-collapsed? (:selection opendoc))
        (and (= qn :doc.api/type-in) (= " " (:key ev)))))
    (correct [_ opendoc]
      (let [z (protocols/zip-to-selection opendoc)]
        (if (and (t/zip-is-material? z) (= (t/zip-semantics z) :text))
          (let [end-pos (peek (t/zip-end-path z))
                to-search (subs (:content (z/node z)) 0 end-pos)
                [start end url] (positions-to-split to-search)]
            (if start
              (let [node (z/node z)
                    text-node-left (when (> start 0) (update node :content subs 0 start))
                    link-node (-> node
                                  (assoc-in [:meta :original-semantics] :text)
                                  (assoc-in [:data :href] url)
                                  (assoc :semantics :link)
                                  (assoc :content url))
                    text-node-right (update node :content subs end)
                    z (if text-node-left (z/insert-left z text-node-left) z)
                    z (z/insert-left z link-node)
                    z (z/replace z (t/set-material-position text-node-right (- end-pos end)))]
                (-> opendoc
                    (assoc :doc (z/root z))
                    (assoc :selection (s/collapsed-selection (t/zip-full-path z)))))
              opendoc))
          opendoc)))))
