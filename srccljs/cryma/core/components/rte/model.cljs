(ns cryma.core.components.rte.model
  (:require
    [clojure.zip :as z]
    [cryma.core.components.rte.model.tools :as t]
    [cryma.core.components.rte.model.selection :as s]
    [cryma.core.components.rte.model.clipboard :as cpbd]
    [cryma.core.components.rte.model.basic-formatting :as basic-formatting]
    [cryma.core.components.rte.model.block-formatting :as block-formatting]
    [cryma.core.components.rte.model.insertion :as insertion]
    [cryma.core.components.rte.model.typing-in :as typing-in]
    [cryma.core.components.rte.model.autocorrectors.protocols :as ac]
    [cryma.core.components.rte.model.history :as h]
    [cryma.core.components.rte.model.configuration :as configuration]
    ))

;;=============================
;; API
;;=============================

(defmulti -do-action (fn [_ ev] (:qn ev)))

(defn generic-result [z opendoc]
  (->
    opendoc
    (assoc :doc (z/root z))
    (s/update-selection (s/zip-collapsed-selection z))))

(defn api-implementation [opendoc ev]
  (let [conf (configuration/get-conf (:doc opendoc))]
    (if-let [custom-impl (get-in conf [:api (:qn ev) :method-implementation])]
      (custom-impl opendoc ev)
      (-do-action opendoc ev))))

(defn apply-auto-correctors [opendoc ev]
  (let [conf (configuration/get-conf (:doc opendoc))]
    (loop [correctors (vals (:auto-correctors conf)) result opendoc]
      (if-let [corrector (first correctors)]
        (recur
          (rest correctors)
          (if (ac/applicable? corrector result ev)
            (ac/correct corrector result) result))
        result))))

(defn do-action [opendoc ev]
  (let [opendoc (t/clear-rendering-meta opendoc)
        r (->
            opendoc
            (api-implementation ev)
            (apply-auto-correctors ev)
            (s/update-selection-state)
            (h/update-history ev opendoc)
            (select-keys [:doc :selection :selection-state :history :clipboard :ui]))]
    r))

(defmethod -do-action :doc.api/type-in
  [opendoc ev]
  (->
    (typing-in/typing-zipper opendoc)
    (typing-in/-process-type-in (:selection opendoc) ev)
    (generic-result opendoc)))

(defmethod -do-action :doc.api/new-line
  [opendoc ev]
  (->
    (typing-in/typing-zipper opendoc)
    (typing-in/-process-new-line (:selection opendoc) ev)
    (generic-result opendoc)))

(defmethod -do-action :doc.api/backspace
  [{:keys [selection] :as opendoc} ev]
  (let [z (typing-in/typing-zipper opendoc)]
    (->
      (if (s/selection-collapsed? selection)
        (typing-in/-process-backspace z selection ev) z)
      (generic-result opendoc))))

(defmethod -do-action :doc.api/del
  [{:keys [selection] :as opendoc} ev]
  (let [z (typing-in/typing-zipper opendoc)]
    (->
      (if (s/selection-collapsed? selection)
        (typing-in/-process-del z selection ev) z)
      (generic-result opendoc))))

(defmethod -do-action :doc.api/basic-formatting
  [{:keys [selection] :as opendoc} ev]
  (if-not (s/selection-collapsed? selection)
    (basic-formatting/process-basic-formatting opendoc ev)
    opendoc))

(defmethod -do-action :doc.api/block-formatting
  [opendoc ev]
  (block-formatting/process-block-formatting opendoc ev))

(defmethod -do-action :doc.api/insert
  [opendoc ev]
  (->
    opendoc
    (insertion/process-insert ev)
    (generic-result opendoc)))

(defmethod -do-action :doc.api/update
  [opendoc ev]
  (->
    opendoc
    (insertion/process-update ev)
    (generic-result opendoc)))

(defmethod -do-action :doc.api/copy
  [{:keys [doc selection] :as opendoc} ev]
  (-> opendoc
      (assoc :clipboard
             (cpbd/process-copy (t/doc-zipper doc) selection ev))))

(defmethod -do-action :doc.api/cut
  [{:keys [doc selection] :as opendoc} ev]
  (let [z (typing-in/typing-zipper opendoc)]
    (->
      (if (s/selection-collapsed? selection)
        (typing-in/-process-del z selection ev) z)
      (generic-result opendoc)
      (assoc :clipboard
             (cpbd/process-copy (t/doc-zipper doc) selection ev)))))

(defmethod -do-action :doc.api/paste
  [{:keys [selection] :as opendoc} ev]
  (-> (typing-in/typing-zipper opendoc)
      (cpbd/process-paste selection ev)
      (generic-result opendoc)))

(defmethod -do-action :doc.api/undo [opendoc _] opendoc)

(defmethod -do-action :doc.api/redo [opendoc _] opendoc)

;;=============================
;; OPEN
;;=============================

(defn open-document
  ([doc]
   (open-document doc nil nil))
  ([doc param]
   (let [at-path (when (vector? param) param)
         conf (when (map? param) param)]
     (open-document doc conf at-path)))
  ([doc conf at-path]
   (let [doc (cond
               (string? doc)
               ((get-in conf [:converters :import]) doc)
               (nil? doc)
               (t/create-empty-document)
               :else
               doc)
         doc (configuration/with-conf doc conf)]
     (s/update-selection
       {:doc     doc
        :history (h/init-history)
        :ui      {}}
       (cond
         (vector? at-path)
         (s/collapsed-selection (s/normalize-path doc at-path))
         (= :end at-path)
         (s/init-selection-end doc)
         :else
         (s/init-selection-start doc))))))
