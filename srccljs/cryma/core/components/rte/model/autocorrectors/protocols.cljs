(ns cryma.core.components.rte.model.autocorrectors.protocols
  (:require
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [cryma.core.components.rte.model.tools :as t]))

(defprotocol IAutoCorrector
  (applicable?
    [this opendoc ev]
    "Determines whether this corrector applicable to current opendoc state.")
  (correct
    [this opendoc]
    "Corrects opendoc tree."))

(defn zip-to-selection [opendoc]
  (t/zip-get-in
    (t/doc-zipper (:doc opendoc))
    (get-in opendoc [:selection :start])))