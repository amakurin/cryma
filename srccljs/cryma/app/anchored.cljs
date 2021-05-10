(ns cryma.app.anchored
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require [cryma.core.components.channels :as ch]
            [cryma.core.components.cursors :as c]
            [cryma.core.api :as api]
            [cryma.core.tools-ui :as tools-ui]
            [rum.core :as rum]
            [cljs.core.async :refer [<! put! >! alts!]]))

(defn internal-anchored-handler [anchored-opts state ev]
  (let [{:keys [authorize-query-key
                anchor-query-key
                ev->value-keys
                agg-attr
                agg-attr-update-key]} anchored-opts
        {:keys [core]} (first (:rum/args state))
        opts (tools-ui/get-opts state)
        authorize-query (authorize-query-key opts)
        anchor-query (anchor-query-key opts)
        anchor (second (:rum/args state))
        anchor-id (:db/id @anchor)]
    (case (:qn ev)
      :anchor
      (go
        (let [response (<! (api/call-async core authorize-query))]
          (if (get-in response [:result :permitted?])
            (ch/alts!stev state (assoc ev :qn :permitted-anchor))
            (api/pub-event core {:qn         :authorization/permission-denied
                                 :denied-msg anchor-query}))))

      :permitted-anchor
      (go
        (let [response (<! (api/call-async
                             core
                             (assoc anchor-query
                               :value (-> ev
                                          (select-keys ev->value-keys)
                                          (assoc :sys/anchor anchor-id)))))]
          (if (api/success? core response)
            (c/swp! anchor
                    (fn [anchor]
                      (-> anchor
                          (update :sys/_anchor
                                  #(vec (conj % (:result response))))
                          (update agg-attr #(+ (or % 0)
                                               (if agg-attr-update-key
                                                 (agg-attr-update-key ev 1) 1)))))))))
      :unanchor
      (go
        (let [response (<! (api/call-async
                             core
                             (assoc anchor-query
                               :value (select-keys ev [:db/id])
                               :retraction? true)))]
          (if (api/success? core response)
            (c/swp! anchor
                    (fn [anchor]
                      (-> anchor
                          (update agg-attr #(- (or % 0)
                                               (if agg-attr-update-key
                                                 (agg-attr-update-key ev 1) 1)))
                          (update :sys/_anchor
                                  #(filterv (fn [anc] (not= (:db/id anc) (:db/id ev))) %))))))))
      nil)))


(rum/defcs
  subscriptions
  <
  (ch/channels :new-root-chan!)
  (ch/simple-chans
    (partial internal-anchored-handler
             {:authorize-query-key :authorize-sub-query
              :anchor-query-key    :sub-query
              :agg-attr            :app/subs}))
  (c/cursored)
  [state module anchor-cursor opts]
  (let [l (:localizer module)
        actual-anchor (c/actual anchor-cursor)
        subbed? (some (fn [ent]
                        (when
                          (= :entity.app/sub (get-in ent [:sys/entity :db/ident]))
                          ent))
                      (:sys/_anchor @actual-anchor))
        actual-value (:app/subs @actual-anchor 0)
        sub-opts {:class    (when subbed? "subbed")
                  :on-click (fn [_]
                              (if (nil? subbed?)
                                (ch/alts!stev state {:qn :anchor})
                                (ch/alts!stev state {:qn :unanchor :db/id (:db/id subbed?)})))}]
    [:span.subs
     [:span.sub sub-opts]
     [:span.sub-count actual-value]
     [:span.join sub-opts (l (if (nil? subbed?) :subs/subscribe :subs/unsubscribe))]]))


(rum/defcs
  favorities
  <
  (ch/channels :new-root-chan!)
  (ch/simple-chans
    (partial internal-anchored-handler
             {:authorize-query-key :authorize-favorite-query
              :anchor-query-key    :favorite-query
              :agg-attr            :app/favorites}))
  (c/cursored)
  [state module anchor-cursor opts]
  (let [l (:localizer module)
        actual-anchor (c/actual anchor-cursor)
        favorite? (some (fn [ent]
                          (when
                            (= :entity.app/favorite (get-in ent [:sys/entity :db/ident]))
                            ent))
                        (:sys/_anchor @actual-anchor))
        actual-value (:app/favorites @actual-anchor 0)
        enabled? (:enabled? opts)]
    [:span.favorites
     [:span.favorite
      {:class    (when favorite? "favorited")
       :title    (l (if (nil? favorite?) :favorities/add :favorities/remove))
       :on-click (fn [_]
                   (when enabled?
                     (if (nil? favorite?)
                       (ch/alts!stev state {:qn :anchor})
                       (ch/alts!stev state {:qn :unanchor :db/id (:db/id favorite?)}))))}]
     [:span.fav-count
      {:title (l :app/favorites)}
      actual-value]]))

(rum/defcs
  ratings
  < (ch/channels :new-root-chan!)
  (ch/simple-chans
    (partial internal-anchored-handler
             {:authorize-query-key :authorize-rate-query
              :anchor-query-key    :rate-query
              :ev->value-keys      [:rating/value]
              :agg-attr            :app/ratings
              :agg-attr-update-key :rating/value}))
  (c/cursored)
  [state module anchor-cursor opts]
  (let [actual-anchor (c/actual anchor-cursor)
        rating? (some (fn [ent]
                        (when
                          (= :entity.app/rating (get-in ent [:sys/entity :db/ident]))
                          ent))
                      (:sys/_anchor @actual-anchor))
        rate? (:rating/value rating?)
        actual-value (:app/ratings @actual-anchor 0)
        enabled? (:enabled? opts)
        positive? (< 0 actual-value)]
    [:span.ratings
     [:span.up
      {:class    (when (= 1 rate?) "rated")
       :title    "+1"
       :on-click (fn [_]
                   (when enabled?
                     (if (nil? rating?)
                       (ch/alts!stev state {:qn :anchor :rating/value 1})
                       (ch/alts!stev state {:qn           :unanchor
                                            :db/id        (:db/id rating?)
                                            :rating/value rate?}))))}]
     [:span.rating
      {:class (if positive? "positive" (when (> 0 actual-value) "negative"))}
      actual-value]

     [:span.down
      {:class    (when (= -1 rate?) "rated")
       :title    "-1"
       :on-click (fn [_]
                   (when enabled?
                     (if (nil? rating?)
                       (ch/alts!stev state {:qn :anchor :rating/value -1})
                       (ch/alts!stev state {:qn           :unanchor
                                            :db/id        (:db/id rating?)
                                            :rating/value rate?}))))}]]))