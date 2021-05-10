(ns cryma.core.components.rte.materials
  (:require
    [cryma.core.components.cursors :as c]
    [rum.core :as rum]
    [cryma.core.components.rte.ui-tools :as uit]
    [cryma.core.components.rte.model.configuration :as conf]
    [cryma.core.components.rte.model.formats :as formats]))

(defn render-string [s]
  (if (empty? s) "\u200B" s))

(rum/defcs
  doc-text
  < (c/cursored)
  [state material]
  (let [format (:format @material)]
    [:span.rte-material
     (merge
       {:data-keks-id (uit/doc-cursor->key material)
        :data-keks    (pr-str [(:type @material) (:semantics @material)])}
       (formats/render-formats format (uit/cursor->conf material)))
     (render-string (:content @material))]))

(rum/defcs
  doc-link
  < (c/cursored)
  [state material]
  (let [{:keys [data meta]} @material
        component (conf/get-semantics
                    (uit/cursor->conf material) (:original-semantics meta) :component)]
    [:a.rte-material
     {:href   (:href data)
      :target "_blank"}
     (component material)]))

(rum/defcs
  doc-img
  < (c/cursored)
  [state material]
  [:span.rte-material
   {:data-keks-id (uit/doc-cursor->key material)
    :data-keks    (pr-str [(:type @material) (:semantics @material)])}
   [:img
    {:src (get-in @material [:data :src])}]])

(rum/defcs
  doc-video
  < (c/cursored)
  [state material]
  [:span.rte-material
   {:key          (str "vc:" (uit/doc-cursor->key material))
    :data-keks-id (uit/doc-cursor->key material)
    :data-keks    (pr-str [(:type @material) (:semantics @material)])
    :dangerouslySetInnerHTML
                  {:__html (get-in @material [:data :embedding-code])}}
   ])