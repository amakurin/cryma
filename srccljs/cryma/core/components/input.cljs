(ns cryma.core.components.input
  (:require
    [cryma.core.components.cursors :as c]
    [cryma.core.components.channels :as ch]
    [cryma.core.components.ui-events :as uiev]
    [cryma.core.components.common :as common]
    [cryma.core.tools-ui :as tools-ui]
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [rum.core :as rum]))

(rum/defcs
  cursored-input
  < (c/cursored)
    common/mix-opts-should-update
  (ch/channels)
  (c/cursored-chans [:rum/args 1])
  [state module cursor {:keys [placeholder mode] :as opts}]
  (let [value @cursor
        has-value? (and (string? value) (seq value))]
    [:span.input-text
     {:class (str
               (when has-value? "with-value")
               (when mode (str " " (name mode))))}
     (when (and has-value? (seq placeholder)
                (not= mode :input.modes/light))
       [:span.ph placeholder])
     [:input
      (merge
        (dissoc opts :mode)
        {:type        (:type opts "text")
         :value       value
         :on-key-down #(ch/alts!stev state (uiev/wrap-event % :on-key-down))
         :on-change   (fn [e]
                        (when-let [change (:on-change opts)]
                          (change e))
                        (c/rst! cursor (.. e -target -value)))})]]))