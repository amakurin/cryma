(ns cryma.app.promo
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require
    [clojure.string :as clostr]
    [cryma.core.tools-ui :as tools-ui]
    [rum.core :as rum]
    [cryma.core.api :as api]
    [cryma.core.components.cursors :as c]
    [cljs.core.async :as async]
    [cryma.core.components.common :as common]
    [cryma.core.components.input :as input]
    [cryma.core.schema :as sch]
    [cryma.core.tools :as tools]))

(defn render-period-item
  [module current-frame-cursor item]
  (let [frames {:time-frames/week        0
                :time-frames/month       1
                :time-frames/three-month 2
                :time-frames/year        3}
        {:keys [localizer]} module
        {:keys [ident discount-text]} item
        frame-pos (get frames ident)
        current-pos (get frames @current-frame-cursor)
        pre? (< frame-pos current-pos)
        active? (= frame-pos current-pos)]
    [:li
     {:class    (cond pre? "pre" active? "active")
      :on-click (fn [_] (c/rst! current-frame-cursor ident))}
     [:div.period-markup
      [:div.border
       [:div.part.first]
       [:div.radio-button]
       [:div.part.second]]]
     [:span.caption
      (str (localizer ident)
           (when (seq discount-text) (str " (" discount-text ")")))]]))

(defn find-amount
  [promo-data product time-frame]
  (when-let [prices (:prices promo-data)]
    (get-in prices [[product time-frame] 1] 0)))

(defn find-price-id
  [promo-data product time-frame]
  (when-let [prices (:prices promo-data)]
    (get-in prices [[product time-frame] 0] 0)))

(defn render-pricing-plan
  [module buy-block-cursor plan]
  (let [{:keys [localizer core]} module
        {:keys [product time-frame included]} plan
        promo-data (:promo-data @buy-block-cursor)
        amount (find-amount promo-data product time-frame)]
    [:li.plan
     [:div.inner
      [:h2.title (localizer product)]
      [:div.price
       [:div.sum
        [:span.currency "$"]
        [:span.amount (or amount "-")]]
       [:div.period (localizer [:for time-frame])]]
      [:div.included-wrapper
       (vec
         (concat
           (->> included
                (map (fn [inc] [:li inc]))
                (concat [:ul.included]))
           (map (fn [_] [:li.empty "  "]) (range 0 (- 4 (count included))))))]
      [:div.buy
       (cond
         (nil? amount)
         [:div.free " - "]
         (> amount 0)
         [:div.buy-button
          {:on-click
           (fn [_]
             (common/scroll-to {:y 0})
             (c/swp! buy-block-cursor assoc
                     :step :buy-block-steps/plan-confirmation
                     :plan product
                     :price-id (find-price-id promo-data product time-frame)))}
          (localizer :buy-button/caption)]
         :else
         [:div.free
          {:on-click
           (fn [_] (api/pub-event core {:qn :action.app/browse-posts}))}
          (localizer :free/caption)])]]]))

(rum/defcs
  plan-selector
  < (c/cursored {})
  [state module buy-block-cursor]
  (let [{:keys [current-time-frame]} buy-block-cursor]
    [:div.buy-block
     [:ul.period-switch
      (render-period-item module
                          current-time-frame
                          {:ident :time-frames/week})
      (render-period-item module
                          current-time-frame
                          {:ident         :time-frames/month
                           :discount-text "\u00a0-25%"})
      (render-period-item module
                          current-time-frame
                          {:ident         :time-frames/three-month
                           :discount-text "\u00a0-30%"})
      (render-period-item module
                          current-time-frame
                          {:ident         :time-frames/year
                           :discount-text "4\u00a0месяца бесплатно"})]
     [:ul.plans
      (render-pricing-plan
        module buy-block-cursor
        {:product    :access-level/omega
         :time-frame :always
         :included   ["Просмотр открытых\u00a0материалов"
                      "Предпросмотр премиум\u00a0материалов"]})
      (render-pricing-plan
        module buy-block-cursor
        {:product    :access-level/zeta
         :time-frame @current-time-frame
         :included   ["Обсуждение,\u00a0оценка открытых\u00a0материалов"
                      "Обсуждение,\u00a0оценка премиум\u00a0материалов"]})
      (render-pricing-plan
        module buy-block-cursor
        {:product    :access-level/gamma
         :time-frame @current-time-frame
         :included   ["Обсуждение,\u00a0оценка открытых\u00a0материалов"
                      "Обсуждение,\u00a0оценка премиум\u00a0материалов"
                      "Просмотр закрытых материалов"]})
      (render-pricing-plan
        module buy-block-cursor
        {:product    :access-level/betta
         :time-frame @current-time-frame
         :included   ["Обсуждение,\u00a0оценка открытых\u00a0материалов"
                      "Обсуждение,\u00a0оценка премиум\u00a0материалов"
                      "Обсуждение,\u00a0оценка закрытых\u00a0материалов"]})
      (render-pricing-plan
        module buy-block-cursor
        {:product    :access-level/alpha
         :time-frame @current-time-frame
         :included   ["Обсуждение,\u00a0оценка открытых\u00a0материалов"
                      "Обсуждение,\u00a0оценка премиум\u00a0материалов"
                      "Обсуждение,\u00a0оценка закрытых\u00a0материалов"
                      "Публикация материалов"]})]]))

(defn prepare-errors [errors]
  (->>
    errors
    (mapcat (fn [err]
              (if (= :order/account-data (:field err))
                (sch/flatten-map-errors [(-> err
                                             (dissoc :field))])
                [err])))
    vec))

(defn select-on-click [e]
  (let [node (.-target e)]
    (when (and (.-getSelection js/window)
               (.-createRange js/document))
      (let [selection (.getSelection js/window)
            range (.createRange js/document)]
        (.selectNodeContents range node)
        (.removeAllRanges selection)
        (.addRange selection range)))))

(rum/defcs
  pay-waiter
  < (c/cursored {:time-counter 0})
    {:did-mount
     (fn [state]
       (let [callback #(let [time-counter (c/local state :time-counter)]
                        (c/swp! time-counter + 2))
             interval (js/setInterval callback 2000)]
         (assoc state ::interval interval)))
     :transfer-state
     (fn [old-state state]
       (merge state (select-keys old-state [::interval])))
     :will-unmount
     (fn [state] (js/clearInterval (::interval state)))}
  [state module pay-request]
  (let [{:keys [core localizer]} module
        counter (c/local state :time-counter)
        passed-secs @counter
        max (api/get-conf module :await-payment-timeout-minutes 60)
        max-secs (* max 60)
        percent (* 100 (/ passed-secs max-secs))
        left-secs (- max-secs passed-secs)
        left-mins (quot left-secs 60)
        left-secs (rem left-secs 60)]
    [:div.progress
     (when (< passed-secs max-secs)
       [:span.status
        (localizer {:code :promo/waiting-for-payment :max-minutes max})])
     (if (>= passed-secs max-secs)
       [:div.expired
        [:div.message
         (localizer :promo/payment-expired-message)]
        [:div.btn.need-more-time
         {:on-click (fn [_]
                      (go
                        (c/rst! counter 0)
                        (async/<!
                          (api/call-async
                            core
                            {:qn          :billing/prolong-order
                             :order/token (:order/token @pay-request)
                             :target      :server}))))}
         (localizer :promo/payment-need-more-time)]]
       [:div.bar
        [:div.inner
         {:style {:width (str percent "%")}}]
        [:span.current-time-left
         (str (localizer {:code      :promo/payment-time-left
                          :left-mins left-mins
                          :left-secs left-secs}))]])
     [:span.instruction
      (localizer :promo/payment-instruction)]]))

(defn paid-status? [status]
  (#{:order-statuses/paid
     :order-statuses/partially-confirmed
     :order-statuses/confirmed
     :order-statuses/forwarded} status))

(defmulti payment-handler (fn [pay-request login pwd module event] (:qn event)))

(defmethod payment-handler :billing/payment-received
  [pay-request login pwd module event]
  (go
    (let [{:keys [core session-reader]} module
          signedin? (= :authenticated (api/read-val session-reader :status))
          new-status (:order-status event)
          _ (c/swp! pay-request assoc
                    :sys/status
                    new-status
                    :total-received
                    (:total-received event)
                    :last-received
                    (:amount-received event))
          response
          (when (and (not signedin?) (paid-status? new-status))
            (async/<! (api/call-async
                        core
                        {:qn          :spa-authenticator/authenticate
                         :credentials {:login @login
                                       :pwd   @pwd}})))]
      (when (api/success? core response)
        (async/<! (async/timeout 2000))
        (api/pub-event
          core
          {:qn            :spa/signed-in
           :principal     (:result response)
           :handled-event {:redirect-event {:qn :action.app/browse-posts}}}))
      (when signedin?
        (async/<! (async/timeout 2000))
        (api/pub-event core {:qn :action.app/browse-posts})))))

(defmethod payment-handler :billing/order-cancelled
  [pay-request login pwd module event]
  (c/swp! pay-request assoc :sys/status :order-statuses/cancelled))

(rum/defcs
  plan-confirmator
  < (c/cursored {:local {:chan        nil
                         :pay-request nil
                         :order       {}}})
    {:will-mount
     (fn [state]
       (let [module (first (:rum/args state))
             core (:core module)
             local (c/local state :local)
             pay-request (:pay-request local)
             order (:order local)
             buy-block-cursor (second (:rum/args state))
             {:keys [:user/login :user/pwd]} (:account buy-block-cursor)
             chan (async/chan 1)]
         (c/swp! local assoc :chan chan)
         (go
           (loop []
             (when-let [event (async/<! chan)]
               (payment-handler pay-request login pwd module event)
               (recur))))
         (go
           (let [response
                 (async/<! (api/call-async
                             core
                             {:qn            :billing/calculate-order
                              :order/product (:plan @buy-block-cursor)
                              :order/product-price
                                             (:price-id @buy-block-cursor)
                              :order/paygate :paygates/btc-gate
                              :target        :server}))]
             (c/rst! order (:result response)))))
       state)
     :will-unmount
     (fn [state]
       (let [module (first (:rum/args state))
             core (:core module)
             local (c/local state :local)
             pay-request (:pay-request local)]
         (when-not (paid-status? (:sys/status @pay-request))
           (go
             (async/<!
               (api/call-async
                 core
                 {:qn          :billing/cancel-order
                  :order/token (:order/token @pay-request)
                  :target      :server}))))
         (when-let [ch (:chan @local)]
           (async/close! ch)
           (c/swp! local dissoc :chan))
         state))}
  [state module buy-block-cursor]
  (let [local (c/local state :local)
        order (:order local)
        pay-request (:pay-request local)
        {:keys [session-reader localizer core]} module
        {:keys [:user/nickname :user/login :user/pwd] :as account-cursor}
        (:account buy-block-cursor)
        signedin? (= :authenticated (api/read-val session-reader :status))
        disabled? (or (= :loading @pay-request) (map? @pay-request))
        {:keys [product time-frame base-amount amount]}
        (c/cur {:product     (localizer (:plan @buy-block-cursor))
                :time-frame  (clostr/capitalize
                               (localizer
                                 (:current-time-frame @buy-block-cursor)))
                :base-amount (or (str "$" (:base-amount @order)) "-")
                :amount      (or (str "$" (:amount @order)) "-")})
        negative-amount? (and (number? (:amount @order)) (< (:amount @order) 0))
        zero? (and (number? (:amount @order)) (= (:amount @order) 0))
        has-discount? (and (number? (:amount @order))
                           (number? (:base-amount @order))
                           (< (:amount @order) (:base-amount @order)))]
    [:div.buy-block.confirmation
     [:ul.form
      [:li.frow.order
       [:span.input-text.with-value
        [:span.ph (localizer :sign-up-product)]
        [:span#product-field.value
         @product]]]
      [:li.frow.order
       [:span.input-text.with-value
        [:span.ph (localizer :sign-up-time-frame)]
        [:span#time-frame-field.value
         @time-frame]]]
      (when has-discount?
        [:li.frow.order
         [:span.input-text.with-value
          [:span.ph (localizer :sign-up-base-amount)]
          [:span#base-amount-field.value
           @base-amount]]])
      [:li.frow.order
       [:span.input-text.with-value
        [:span.ph (localizer :sign-up-amount)]
        [:span#amount-field.value
         {:class (when negative-amount? "negative")}
         @amount]]]
      (when negative-amount?
        [:li.frow.order
         [:div.negative-notice
          (localizer :promo/negative-notice)]])
      (when negative-amount?
        [:li.frow
         [:div.btn
          {:on-click (fn [_] (api/pub-event core {:qn :spa/sign-up}))}
          (localizer :promo/negative-notice-back)]])
      (when-not signedin?
        [:li.account-data
         [:ul.form
          (common/wrap-frow
            module :user/nickname account-cursor
            (input/cursored-input
              module nickname
              {:placeholder (localizer :sign-up-nick-placeholder)
               :id          "nick-field"
               :disabled    disabled?
               :maxLength   20
               :on-change   (fn [e]
                              (when (or (empty? @login)
                                        (= @nickname @login))
                                (c/rst! login (.-value (.-target e)))))}))
          (common/wrap-frow
            module :user/login account-cursor
            (input/cursored-input
              module login
              {:placeholder (localizer :sign-up-login-placeholder)
               :id          "login-field"
               :disabled    disabled?
               :maxLength   20}))
          (common/wrap-frow
            module :user/pwd account-cursor
            (input/cursored-input
              module pwd
              {:placeholder (localizer :sign-up-pwd-placeholder)
               :id          "pwd-field"
               :type        "password"
               :disabled    disabled?
               :maxLength   20}))]])
      (when (not (map? @pay-request))
        [:li.frow
         [:span.btn.pay
          {:class    (when disabled? "disabled")
           :on-click (fn [_]
                       (go
                         (c/swp! account-cursor dissoc :errors)
                         (let [response
                               (async/<!
                                 (api/call-async
                                   core
                                   (->
                                     {:qn            :billing/process-order
                                      :order/product (:plan @buy-block-cursor)
                                      :order/product-price
                                                     (:price-id @buy-block-cursor)
                                      :order/paygate :paygates/btc-gate
                                      :target        :server}
                                     (tools/->if (not signedin?)
                                                 assoc
                                                 :order/account-data
                                                 {:user/login    @login
                                                  :user/pwd      @pwd
                                                  :user/nickname @nickname}))))]
                           (if (api/success? core response)
                             (let [pr (:result response)]
                               (c/rst! pay-request (update pr :sys/status :db/ident))
                               (if (paid-status? (:sys/status @pay-request))
                                 (do
                                   (async/<! (async/timeout 2000))
                                   (api/pub-event core {:qn :action.app/browse-posts}))
                                 (api/subscribe-event
                                   core
                                   {:module     module
                                    :msg-filter {:qn       #{:billing/order-cancelled
                                                             :billing/payment-received}
                                                 :order-id (:db/id pr)}
                                    :channel    (:chan @local)})))
                             (let [errors (prepare-errors (:errors response))]
                               (c/swp! account-cursor assoc
                                       :errors errors)
                               (c/rst! pay-request [:error errors]))))))}
          (if (or negative-amount? zero?)
            (localizer :sign-up-confirm)
            (localizer :sign-up-pay-button))]])
      (when (#{:order-statuses/pending
               :order-statuses/partially-paid}
              (:sys/status @pay-request))
        [:li.frow
         (cond
           (= :loading @pay-request)
           [:div.loading (localizer :loading)]
           (and (vector? @pay-request) (= :error (first @pay-request)))
           [:div.error (localizer :error)]
           (map? @pay-request)
           (let [partial? (= :order-statuses/partially-paid (:sys/status @pay-request))
                 total-amount (:order/paygate-amount @pay-request)
                 total-received (or (:total-received @pay-request) 0)
                 amount (if partial? (- total-amount total-received) total-amount)
                 address (:order/paygate-order-ref @pay-request)]
             [:div.pay-request
              (when partial?
                [:div.partially-paid
                 [:span.message
                  (localizer {:code           :promo/partially-paid-message
                              :total-received total-received})]])
              [:span.pay-exactly (localizer :promo/pay-exactly)]
              [:span.paygate-amount
               [:span.amount {:on-click select-on-click} (js/parseFloat (.toFixed amount 9))]]
              [:span.pay-exactly (localizer :promo/to-btc-address)]
              [:span.paygate-address
               [:span.address {:on-click select-on-click} address]]
              (pay-waiter module pay-request)
              [:div.qr
               [:img {:src (str "/qr-service/" address "/" amount "/?size=128")}]]]))])
      (when (paid-status? (:sys/status @pay-request))
        (if signedin?
          (if (or negative-amount? zero?)
            [:div.paid
             [:div.message (localizer :promo/applied-message)]]
            [:div.paid
             [:div.message (localizer :promo/paid-message)]])
          [:div.paid
           [:div.message (localizer :promo/paid-message)]
           [:div.auth (localizer :promo/paid-auth-message)]]))
      (when (= :order-statuses/cancelled (:sys/status @pay-request))
        [:div.cancelled
         [:div.message (localizer :promo/cancelled-message)]])
      ]]))


(rum/defcs
  promo-page
  < (c/cursored {:local {:step               :buy-block-steps/plan-selection
                         :plan               nil
                         :promo-data         nil
                         :price-id           nil
                         :account            {:user/nickname ""
                                              :user/login    ""
                                              :user/pwd      ""}
                         :current-time-frame :time-frames/month}})
    {:will-mount
     (fn [state]
       (let [module (first (:rum/args state))
             core (:core module)
             local (c/local state :local)]
         (go
           (let [response
                 (async/<! (api/call-async
                             core
                             {:qn     :promo/get-promo-data
                              :target :server}))]
             (c/swp! local assoc :promo-data (:result response)))))
       state)}
  [state module data]
  (let [local (c/local state :local)
        step (:step @local)]
    [:div.promo.page
     [:h1 "Доступ к дискуссионной площадке"]
     (case step
       :buy-block-steps/plan-selection
       (plan-selector module local)
       :buy-block-steps/plan-confirmation
       (plan-confirmator module local))
     (:faq (:promo-data @local))]))

(defn handle-denied [module event]
  (let [signedin? (= :authenticated (api/read-val (:session-reader module) :status))
        spa (:spa module)
        l (:localizer module)
        denied-event (:denied-msg event)]
    (api/show-message-box
      spa
      {:text (l {:code          :promo/higher-access-required
                 :denied-action (:qn denied-event)})
       :options
             (vec (remove nil?
                          [{:caption (l :spa-authenticator/sign-up)
                            :event   {:qn :spa/sign-up}}
                           (when-not signedin?
                             {:caption (l :spa-authenticator/sign-in)
                              :event   {:qn             :spa/sign-in
                                        :redirect-event (when (= :pipe (:generated-by event))
                                                          denied-event)}})]))})))

(rum/defcs
  embedded-promo
  [state module data]
  (let [{:keys [session-reader localizer core]} module
        signedin? (= :authenticated (api/read-val session-reader :status))
        {:keys [action]} (tools-ui/get-opts state)]
    [:div.promo-wrapper
     [:ul.form.promo
      [:li.frow
       [:p (localizer [:promo/embedded-message action])]]
      [:li.frow.promo-controls
       [:ul
        [:li [:span.btn.sign-up
              {:on-click #(api/pub-event
                           core
                           {:qn :spa/sign-up})}
              (localizer :spa-authenticator/sign-up)]]
        (when-not signedin?
          [:li [:span.btn.sign-in
                {:on-click #(api/pub-event
                             core
                             {:qn :spa/sign-in})}
                (localizer :spa-authenticator/sign-in)]])]]]]))

(defrecord PromoModule []
  api/IModule
  api/IComponent
  (start [module] module)
  (stop [module] module))

(defn new-promo-module [& [m]]
  (map->PromoModule
    (merge
      {api/id-key          :promo
       api/static-deps-key [:localizer :core :spa :session-reader]
       api/event-subs-key  [{:msg-filter :authorization/permission-denied
                             :handler    handle-denied}]
       api/routes-key      [{:msg-filter      :spa/sign-up
                             :route-component promo-page}]}
      m)))