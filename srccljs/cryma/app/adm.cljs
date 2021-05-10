(ns cryma.app.adm
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cryma.core.components.cursors :as c]
    [cryma.core.components.channels :as ch]
    [cryma.core.components.input :as input]
    [cryma.core.components.browser :as browser]
    [rum.core :as rum]
    [cryma.core.components.common :as common]
    [cljs.core.async :as async :refer [<! put! >! alts!]]
    [cryma.core.api :as api]
    [cryma.core.tools-ui :as tools-ui]
    [cryma.app.users :as users]
    [cryma.core.components.select :as select]
    [cryma.core.tools :as tools]
    [cryma.core.schema :as sch]
    [cryma.core.dtl.core :as dtl]
    [cryma.core.dtl.transformers.str :as str]
    [cryma.core.components.rtext.extensions :as rte-ext]
    [cryma.core.components.rte.rte-full :as rte-full]
    [cryma.core.components.html :as html]))

(def menu-items-key :adm/menu-items)
(def menu-group-key :adm/menu-group-id)

(defn authenticated? [session-reader]
  (= :authenticated (api/read-val session-reader :status)))

(defn get-principal [session-reader & [k]]
  (api/read-val session-reader (filterv some? [:principal k])))

(defn prepare-errors [errors]
  (->>
    errors
    (mapcat (fn [err]
              (if (= :value (:field err))
                (sch/flatten-map-errors [(dissoc err :field)])
                [err])))
    vec))

;====================================================================================
; USERS ADM
;====================================================================================
(def user-keys
  [:db/id
   :user/access-level
   :user/access-expiry
   :user/login
   :user/nickname
   :user/pwd
   :user/about])

(defn user-item-cursor-prepare [item-cursor]
  (c/cur
    {:qn               :action.adm/read-user
     :filter           {:db/id (:db/id @item-cursor)}
     :sys/cached-value @item-cursor}))

(defn upsert-user [cursor]
  (let [operation-action (if (get-in @cursor [:value :db/id])
                           :action.adm/update-user
                           :action.adm/create-user)]

    (-> @cursor
        (update :value
                (fn [value]
                  (-> value
                      (update :user/access-level first)
                      (update :user/access-expiry (fn [d]
                                                    (when (and d (string? d) (seq d))
                                                      (let [parsed (.parse js/Date d)]
                                                        (if (and (re-find #"^\d\d\d\d-\d\d-\d\d$" d)
                                                                 (not (js/isNaN parsed)))
                                                          (js/Date. parsed)
                                                          d)))))
                      (update :user/about
                              (fn [body]
                                (when body
                                  (let [body (html/trim body)]
                                    (if (html/empty-html? body)
                                      "" body)))))
                      (tools/->if (nil? (seq (:user/pwd value))) dissoc :user/pwd)
                      (tools/->if (nil? (:user/about value)) dissoc :user/about)
                      (tools/->if (nil? (:user/access-expiry value)) dissoc :user/access-expiry)
                      (select-keys user-keys)
                      tools/prepare-value)))
        (assoc :qn operation-action)
        (select-keys [:qn :value])
        (assoc :target :server))))

(rum/defcs
  user-editor
  < (c/cursored {:local {}})
    (ch/channels)
    (c/cursored-chans
      [:rum/args 1]
      (fn [state ev]
        (let [module (first (:rum/args state))
              {:keys [core localizer]} module
              cursor (second (:rum/args state))
              ch4write (ch/ch4write state)]
          (case (:qn ev)

            :user-editor/mount
            (if (= :action.adm/update-user (:qn @cursor))
              (ch/alts!ev ch4write {:qn :user-editor/query-value})
              (c/swp! cursor update :value (fn [v] (if v v {}))))

            :user-editor/query-value
            (do
              (c/swp! cursor assoc :value :loading)
              (async/pipeline
                1
                ch4write
                (map (fn [response] {:qn       :user-editor/finish-query-value
                                     :response response}))
                (api/call-async
                  core
                  (->
                    (common/event->request @cursor)
                    (dissoc :sys/guid :value)
                    (assoc
                      :qn :action.adm/read-user
                      :target :server)))
                false))

            :user-editor/authorize-edition
            (go
              (let [value (:value ev)
                    response
                    (<! (api/call-async
                          core {:qn                 :authorizer/check-permission
                                :action-id          :action.adm/update-user
                                :access-category-id :access-category/administrative
                                :sys/author         (get-in value [:sys/author :db/id])}))]
                (if (and (api/success? core response)
                         (get-in response [:result :permitted?]))
                  (c/swp! cursor assoc :value
                          (-> value
                              (select-keys user-keys)
                              (update :user/access-level vector)
                              (update :user/access-expiry
                                      (fn [d] (when d
                                                (dtl/transform
                                                  (tools/date>str d)
                                                  ["%04d-%02d-%02d" [:year :month :day]]
                                                  str/transformer))))))
                  (let [errors (if (seq (:errors response))
                                 (:errors response)
                                 [{:code       :authorization/permission-denied
                                   :denied-msg {:qn :action.adm/update-user}}])]
                    (c/swp! cursor assoc :value [:error errors])))))

            :user-editor/finish-query-value
            (let [response (:response ev)]
              (if (api/success? core response)
                (ch/alts!ev ch4write {:qn    :user-editor/authorize-edition
                                      :value (:result response)})
                (c/swp! cursor assoc :value [:error (:errors response)])))

            nil))))
    {:will-mount
     (fn [state]
       (ch/alts!stev state {:qn :user-editor/mount})
       state)}
  [state module cursor]
  (let [{:keys [localizer core]} module
        local (c/local state :local)
        actual (c/actual cursor)
        value (:value actual)
        error? (and (vector? @value) (= :error (first @value)))
        loading? (or (= :loading @value) (nil? value) (nil? @value))]
    [:div
     {:class (if (= :action.adm/update-user (:qn @cursor)) "page user-edit" "user-edit")}
     (cond
       loading?
       [:div.loading (localizer :loading)]
       error?
       [:div.error (localizer (get-in @value [1 0] :error))]
       value
       [:ul.form
        {:ref "edit"}

        (common/wrap-frow
          module :user/login local
          (input/cursored-input
            module
            (:user/login value)
            {:placeholder (localizer :user/login)}))

        (common/wrap-frow
          module :user/nickname local
          (input/cursored-input
            module
            (:user/nickname value)
            {:placeholder (localizer :user/nickname)}))

        (common/wrap-frow
          module :user/pwd local
          (input/cursored-input
            module
            (:user/pwd value)
            {:placeholder (localizer :user/pwd)
             :id          "pwd-field"
             :type        "password"
             :maxLength   20}))

        (common/wrap-frow
          module :user/access-level local
          (select/cursored-multi-select
            module
            (:user/access-level value)
            {:drop-down-item-renderer (fn [item]
                                        [:span
                                         {:class (-> item :db/ident name)}
                                         [:span.name (or
                                                       (:access-level/name item)
                                                       (localizer (:db/ident item)))]])
             :max-selected-count      1
             :item-renderer           (fn [item] [:span
                                                  {:title (:db/doc item)
                                                   :class (str "access-level "
                                                               (-> item :db/ident name))}
                                                  (or
                                                    (:access-level/name item)
                                                    (localizer (:db/ident item)))])
             :placeholder             (localizer :user/access-level)
             :query-template          {:qn            :action.adm/access-level-search
                                       :filter        {:access-level/assignable? true}
                                       :search-string ""
                                       :order         [:db/ident]
                                       :limit         50
                                       :pattern       [:db/id :db/ident
                                                       :access-level/name :db/doc]
                                       :target        :server}}))
        (common/wrap-frow
          module :user/access-expiry local
          (input/cursored-input
            module
            (:user/access-expiry value)
            {:placeholder (str (localizer :user/access-expiry) "(yyyy-mm-dd)")}))

        (common/wrap-frow
          module :user/about local
          (rte-full/rte-full (:user/about value) (rte-ext/full-rte-opts module (localizer :user/about-title))))

        [:li.frow.control-btns
         [:span.btn
          {:class    "cancel"
           :on-click (fn [_] (api/pub-event core {:qn :navigation/prev}))}
          (localizer :user-editor/cancel)]
         [:span.btn
          {:class "save"
           :on-click
                  (fn [_]
                    (c/swp! local dissoc :errors)
                    (go
                      (let [response
                            (<! (api/call-async
                                  core
                                  (upsert-user actual)))]
                        (if (api/success? core response)
                          (api/pub-event core {:qn :navigation/prev})
                          (c/swp! local assoc :errors (prepare-errors (:errors response)))))))}
          (localizer :user-editor/save)]]
        (common/render-errors module (filter #(-> % :field nil?) (:errors @local)))])]))

(defn user-reader [module cursor {:keys [mode]}]
  (let [{:keys [localizer]} module
        browse? (= mode :item-renderer.modes/browser-embedded)
        user (if browse? (:sys/cached-value @cursor) (:value @cursor))
        about (:user/about user)
        expiry-date (:user/access-expiry user)
        days-left (tools/days-between (js/Date.) expiry-date)
        created-date (:sys/date user)
        author (:sys/author user)]
    [:div.profile.adm
     [:ul.form.info
      [:li.frow
       [:span.prop (str (localizer :db/id) ":")]
       [:span.val (:db/id user)]]
      [:li.frow
       [:span.prop (str (localizer :user/nickname) ":")]
       [:span.val (:user/nickname user)]]
      [:li.frow
       [:span.prop (str (localizer :user/login) ":")]
       [:span.val (:user/login user)]]
      [:li.frow.registered
       [:span.prop (str (localizer :user/registered) ":")]
       [:span.val (str
                    (when created-date
                      (tools/date>str
                        created-date
                        {:l           localizer
                         :format-code :date/no-time}))
                    (when author
                      (str " by " (:user/nickname author))))]]
      [:li.frow.access-level
       [:span.prop (str (localizer :user/access-level) ":")]
       [:span.val
        (common/event-link
          module
          {:qn :spa/sign-up} {}
          (localizer (get-in user [:user/access-level :db/ident])))]
       (when (and days-left (< days-left 0))
         [:span.val.expired (str "(" (localizer :access-expiry/expired) ")")])]

      [:li.frow.access-expiry
       [:span.prop (str (localizer :user/access-expiry) ":")]
       [:span.val (if expiry-date
                    (tools/date>str expiry-date
                                    {:l           localizer
                                     :format-code :date/no-time})
                    (localizer :access-expiry/never))]
       (when (and days-left (>= days-left 0))
         [:span.val.days-left (str ", " (localizer {:code      :access-expiry/days-left
                                                    :days-left days-left}))])]
      [:li.frow
       [:span.prop (str (localizer :links) ":")]
       [:span.val [:ul.links
                   [:li (common/event-link
                          module {:qn     :action.app/read-user-profile
                                  :filter {:db/id (:db/id user)}} {}
                          (localizer :app.adm/user-profile-link))]
                   [:li (common/event-link
                          module {:qn     :action.adm/update-user
                                  :filter {:db/id (:db/id user)}} {}
                          (localizer :app.adm/user-edit-link))]
                   [:li (common/event-link
                          module {:qn            :action.adm/billing
                                  :browse-mode   :billing-orders
                                  :search-string (str (:db/id user))} {}
                          (localizer :app.adm/user-billing-link))]]]]
      ]]))

(rum/defcs
  users-dashboard
  < (c/cursored {})
    (ch/channels)
    (c/cursored-chans [:rum/args 1])
  [state module cursor]
  (let [{:keys [localizer]} module
        cursor (c/actual cursor)
        edition? (= :action.adm/update-user (:qn @cursor))
        browse-mode (:browse-mode @cursor :search)]
    [:div.page.adm.users
     [:h1 (localizer :app.adm/users)]
     (when-not edition?
       (common/browser-head module cursor
                            {:default-mode :search
                             :browse-modes [{:mode-id   :search
                                             :mode-view browser/search-widget}
                                            {:mode-id :new-user}]}))
     (when-not edition?
       (if (#{:search} browse-mode)
         (browser/cursored-browser
           module cursor
           {:item-renderer       user-reader
            :item-cursor-prepare user-item-cursor-prepare
            :query-defaults      {:limit         50
                                  :offset        0
                                  :search-string ""
                                  :browse-mode   browse-mode
                                  :target        :server
                                  :filter        {:user/built-in {:op :not :v true}}}
            :query               {:qn :action.adm/browse-users}})
         (user-editor module cursor)))
     (when edition?
       (user-editor module cursor))
     ]))

;====================================================================================
; HUBS ADM
;====================================================================================
(def hub-keys
  [:db/id :sys/access-category :hub/name :hub/description])

(defn hub-item-cursor-prepare [item-cursor]
  (c/cur
    {:qn               :action.adm/read-hub
     :filter           {:db/id (:db/id @item-cursor)}
     :sys/cached-value @item-cursor}))

(defn upsert-hub [cursor]
  (let [operation-action (if (get-in @cursor [:value :db/id])
                           :action.adm/update-hub
                           :action.adm/create-hub)]

    (-> @cursor
        (update :value
                (fn [value]
                  (-> value
                      (update :sys/access-category first)
                      (select-keys hub-keys)
                      tools/prepare-value)))
        (assoc :qn operation-action)
        (select-keys [:qn :value])
        (assoc :target :server))))

(rum/defcs
  hub-editor
  < (c/cursored {:local {}})
    (ch/channels)
    (c/cursored-chans
      [:rum/args 1]
      (fn [state ev]
        (let [module (first (:rum/args state))
              {:keys [core localizer]} module
              cursor (second (:rum/args state))
              ch4write (ch/ch4write state)]
          (case (:qn ev)

            :hub-editor/mount
            (if (= :action.adm/update-hub (:qn @cursor))
              (ch/alts!ev ch4write {:qn :hub-editor/query-value})
              (c/swp! cursor update :value (fn [v] (if v v {}))))

            :hub-editor/query-value
            (do
              (c/swp! cursor assoc :value :loading)
              (async/pipeline
                1
                ch4write
                (map (fn [response] {:qn       :hub-editor/finish-query-value
                                     :response response}))
                (api/call-async
                  core
                  (->
                    (common/event->request @cursor)
                    (dissoc :sys/guid :value)
                    (assoc
                      :qn :action.adm/read-hub
                      :target :server)))
                false))

            :hub-editor/authorize-edition
            (go
              (let [value (:value ev)
                    response
                    (<! (api/call-async
                          core {:qn                 :authorizer/check-permission
                                :action-id          :action.adm/update-hub
                                :access-category-id :access-category/administrative
                                :sys/author         (get-in value [:sys/author :db/id])}))]
                (if (and (api/success? core response)
                         (get-in response [:result :permitted?]))
                  (c/swp! cursor assoc :value
                          (-> value
                              (update :sys/access-category vector)
                              (select-keys hub-keys)))
                  (let [errors (if (seq (:errors response))
                                 (:errors response)
                                 [{:code       :authorization/permission-denied
                                   :denied-msg {:qn :action.adm/update-hub}}])]
                    (c/swp! cursor assoc :value [:error errors])))))

            :hub-editor/finish-query-value
            (let [response (:response ev)]
              (if (api/success? core response)
                (ch/alts!ev ch4write {:qn    :hub-editor/authorize-edition
                                      :value (:result response)})
                (c/swp! cursor assoc :value [:error (:errors response)])))

            nil))))
    {:will-mount
     (fn [state]
       (ch/alts!stev state {:qn :hub-editor/mount})
       state)}
  [state module cursor]
  (let [{:keys [localizer core]} module
        local (c/local state :local)
        actual (c/actual cursor)
        value (:value actual)
        error? (and (vector? @value) (= :error (first @value)))
        loading? (or (= :loading @value) (nil? value) (nil? @value))]
    [:div
     {:class (if (= :action.adm/update-hub (:qn @cursor)) "page hub-edit" "hub-edit")}
     (cond
       loading?
       [:div.loading (localizer :loading)]
       error?
       [:div.error (localizer (get-in @value [1 0] :error))]
       value
       [:ul.form
        {:ref "edit"}

        (common/wrap-frow
          module :hub/name local
          (input/cursored-input
            module
            (:hub/name value)
            {:placeholder (localizer :hub/name)}))

        (common/wrap-frow
          module :hub/description local
          (input/cursored-input
            module
            (:hub/description value)
            {:placeholder (localizer :hub/description)}))

        (common/wrap-frow
          module :sys/access-category local
          (select/cursored-multi-select
            module
            (:sys/access-category value)
            {:drop-down-item-renderer (fn [item]
                                        [:span
                                         {:class (-> item :db/ident name)}
                                         [:span.name (str (localizer (:db/ident item))
                                                          " (" (:db/doc item) ")")]])
             :max-selected-count      1
             :item-renderer           (fn [item] [:span
                                                  {:title (:db/doc item)
                                                   :class (str "access-category "
                                                               (-> item :db/ident name))}
                                                  (str (localizer (:db/ident item))
                                                       " (" (:db/doc item) ")")])
             :placeholder             (localizer :sys/access-category)
             :query-template          {:qn            :action.adm/access-category-search
                                       :search-string ""
                                       :order         [:db/ident]
                                       :limit         50
                                       :pattern       [:db/id :db/ident :db/doc]
                                       :target        :server}}))

        [:li.frow.control-btns
         [:span.btn
          {:class    "cancel"
           :on-click (fn [_] (api/pub-event core {:qn :navigation/prev}))}
          (localizer :hub-editor/cancel)]
         [:span.btn
          {:class "save"
           :on-click
                  (fn [_]
                    (c/swp! local dissoc :errors)
                    (go
                      (let [response
                            (<! (api/call-async
                                  core
                                  (upsert-hub actual)))]
                        (if (api/success? core response)
                          (api/pub-event core {:qn :navigation/prev})
                          (c/swp! local assoc :errors (prepare-errors (:errors response)))))))}
          (localizer :hub-editor/save)]]
        (common/render-errors module (filter #(-> % :field nil?) (:errors @local)))
        ])]))

(defn hub-reader [module cursor {:keys [mode]}]
  (let [{:keys [localizer]} module
        browse? (= mode :item-renderer.modes/browser-embedded)
        hub (if browse? (:sys/cached-value @cursor) (:value @cursor))
        created-date (:sys/date hub)
        author (:sys/author hub)]
    [:div.profile.adm
     [:ul.form.info
      [:li.frow
       [:span.prop (str (localizer :hub/name) ":")]
       [:span.val (:hub/name hub)]]
      [:li.frow
       [:span.prop (str (localizer :hub/description) ":")]
       [:span.val (:hub/description hub)]]
      [:li.frow.registered
       [:span.prop (str (localizer :sys/date) ":")]
       [:span.val (str
                    (when created-date
                      (tools/date>str
                        created-date
                        {:l           localizer
                         :format-code :date/no-time}))
                    (when author
                      (str " by " (:user/nickname author))))]]
      [:li.frow.access-category
       [:span.prop (str (localizer :sys/access-category) ":")]
       [:span.val (localizer (get-in hub [:sys/access-category :db/ident]))]]

      [:li.frow
       [:span.prop (str (localizer :links) ":")]
       [:span.val [:ul.links
                   [:li (common/event-link
                          module {:qn     :action.app/read-hub
                                  :filter {:db/id (:db/id hub)}} {}
                          (localizer :app.adm/hub-link))]
                   [:li (common/event-link
                          module {:qn     :action.adm/update-hub
                                  :filter {:db/id (:db/id hub)}} {}
                          (localizer :app.adm/hub-edit-link))]]]]
      ]]))

(rum/defcs
  hubs-dashboard
  < (c/cursored {})
    (ch/channels)
    (c/cursored-chans [:rum/args 1])
  [state module cursor]
  (let [{:keys [localizer]} module
        cursor (c/actual cursor)
        edition? (= :action.adm/update-hub (:qn @cursor))
        browse-mode (:browse-mode @cursor :search)]
    [:div.page.adm.hubs
     [:h1 (localizer :app.adm/hubs)]
     (when-not edition?
       (common/browser-head module cursor
                            {:default-mode :search
                             :browse-modes [{:mode-id   :search
                                             :mode-view browser/search-widget}
                                            {:mode-id :new-hub}]}))
     (when-not edition?
       (if (#{:search} browse-mode)
         (browser/cursored-browser
           module cursor
           {:item-renderer       hub-reader
            :item-cursor-prepare hub-item-cursor-prepare
            :query-defaults      {:limit         50
                                  :offset        0
                                  :search-string ""
                                  :browse-mode   browse-mode
                                  :target        :server}
            :query               {:qn :action.adm/browse-hubs}})
         (hub-editor module cursor)))
     (when edition?
       (hub-editor module cursor))
     ]))

;====================================================================================
; BILLING ADM
;====================================================================================
(defn order-item-cursor-prepare [item-cursor]
  (c/cur
    {:qn               :action.adm/billing-read-order
     :filter           {:db/id (:db/id @item-cursor)}
     :sys/cached-value @item-cursor}))

(defn str-amount [amount currency]
  (str amount " " (:currency/letter-code currency)))

(defn order-reader [module cursor {:keys [mode]}]
  (let [{:keys [core localizer]} module
        browse? (= mode :item-renderer.modes/browser-embedded)
        actual (c/actual (if browse? (:sys/cached-value cursor) (:value cursor)))
        {:keys
             [:base-currency
              :order/amount
              :order/token
              :order/account
              :order/product
              :order/product-price
              :order/paygate
              :order/paygate-order-ref
              :order/paygate-amount
              :order/paygate-received-amount
              :order/paygate-confirmed-amount
              :order/paygate-forwarded-amount
              :sys/date
              :sys/status]
         :as order} @actual
        btc? (= (get-in order [:order/paygate :db/ident]) :paygates/btc-gate)
        paygate-currency (:paygate/currency paygate)
        status-ident (:db/ident status)]
    [:div.profile.adm
     [:ul.form.info
      [:li.frow
       [:span.prop (str (localizer :order/account) ":")]
       [:span.val (common/event-link
                    module
                    {:qn            :action.adm/users
                     :browse-mode   :search
                     :search-string (str account)}
                    {}
                    account)]]
      [:li.frow
       [:span.prop (str (localizer :order/product) ":")]
       [:span.val (str (localizer (:product/service-key product))
                       ", " (localizer (:db/ident (:product-price/time-frame product-price)))
                       ", " (str-amount (:product-price/value product-price) base-currency))]]
      [:li.frow
       [:span.prop (str (localizer :sys/date) ":")]
       [:span.val (when date
                    (tools/date>str
                      date
                      {:l           localizer
                       :format-code :date/no-time}))]]
      [:li.frow
       [:span.prop (str (localizer :order/amount) ":")]
       [:span.val (str-amount amount base-currency)]]
      [:li.frow
       [:span.prop (str (localizer :order/paygate-order-ref) ":")]
       [:span.val paygate-order-ref]]
      (when paygate-amount
        [:li.frow
         [:span.prop (str (localizer :order/paygate-amount) ":")]
         [:span.val (str-amount paygate-amount paygate-currency)]])
      (when paygate-received-amount
        [:li.frow
         [:span.prop (str (localizer :order/paygate-received-amount) ":")]
         [:span.val (str-amount paygate-received-amount paygate-currency)]])
      (when paygate-confirmed-amount
        [:li.frow
         [:span.prop (str (localizer :order/paygate-confirmed-amount) ":")]
         [:span.val (str-amount paygate-confirmed-amount paygate-currency)]])
      (when paygate-forwarded-amount
        [:li.frow
         [:span.prop (str (localizer :order/paygate-forwarded-amount) ":")]
         [:span.val (str-amount paygate-forwarded-amount paygate-currency)]])
      [:li.frow
       [:span.prop (str (localizer :sys/status) ":")]
       [:span.val
        [:span {:class (when status-ident (str "order-status " (name status-ident)))} (:db/doc status)]
        (when (not= :order-statuses/cancelled status-ident)
          [:span.order-action.cancel-order
           {:on-click (fn [_]
                        (go
                          (let [response
                                (<! (api/call-async
                                      core
                                      {:qn          :billing/cancel-order
                                       :order/token token
                                       :target      :server}))]
                            (when (api/success? core response)
                              (c/swp! actual assoc :sys/status (get-in response [:result :sys/status]))))))}
           (localizer :order/cancel)])]]
      [:li.frow
       [:span.prop (str (localizer :links) ":")]
       [:span.val
        [:ul.links
         (when (and btc? paygate-order-ref)
           [:li
            [:a.address
             {:target "_blank"
              :href   (str "https://blockexplorer.com/address/" paygate-order-ref)}
             "blockexplorer.com"]])
         (when (and btc? paygate-order-ref)

           [:li
            [:a.address
             {:target "_blank"
              :href   (str "https://blockchain.info/address/" paygate-order-ref)}
             "blockchain.info"]])]]]
      ]]))

(rum/defcs balance-manager
  < (c/cursored {})
    {:will-mount
     (fn [state]
       (let [[module cursor] (:rum/args state)
             {:keys [core]} module]
         (go
           (c/swp! cursor assoc :value :loading)
           (let [response (<! (api/call-async
                                core
                                {:qn     :action.adm/billing-get-balance
                                 :target :server}))
                 result (:result response)]
             (if (api/success? core response)
               (c/swp! cursor assoc :value (:balance result) :network (:network result))
               (c/swp! cursor assoc :value [:error (:errors response)])))))
       state)}
  [state module cursor]
  (let [{:keys [core localizer]} module
        {:keys [available spendable]
         :as   value} (:value @cursor)
        loading? (or (nil? value) (= :loading value))
        error? (and (vector? value) (= :error (first value)))]
    (cond
      loading?
      [:div.loading (localizer :loading)]
      error?
      [:div.error (localizer (get-in value [1 0] :error))]
      :else
      [:div.profile.adm
       [:ul.form.info
        [:li.frow
         [:span.prop (str (localizer :balance/network) ":")]
         [:span.val (localizer (:network @cursor))]]
        [:li.frow
         [:span.prop (str (localizer :balance/available) ":")]
         [:span.val (str available)]]
        [:li.frow
         [:span.prop (str (localizer :balance/spendable) ":")]
         [:span.val
          [:span (str spendable)]
          [:span.order-action.force-forwarding
           {:on-click
            (fn [_]
              (go
                (let [response
                      (<! (api/call-async
                            core
                            {:qn     :action.adm/billing-force-forwarding
                             :target :server}))]
                  (if (api/success? core response)
                    (c/swp! cursor
                            (fn [v] (let [result (:result response)]
                                      (assoc v :value (:balance result)
                                               :forwarded-amount (:forwarded-amount result)))))
                    (c/swp! cursor assoc :value [:error (:errors response)])))))}
           (localizer :balance/force-forwarding)]
          (when-let [forwarded (:forwarded-amount @cursor)]
            [:span (str " " (localizer :balance/forwarded) ": " forwarded)])]]]])))

(rum/defcs
  billing-dashboard
  < (c/cursored {})
    (ch/channels)
    (c/cursored-chans [:rum/args 1])
  [state module cursor]
  (let [{:keys [localizer]} module
        cursor (c/actual cursor)
        browse-mode (:browse-mode @cursor :billing-orders)]
    [:div.page.adm.billing
     [:h1 (localizer :app.adm/billing)]
     (common/browser-head module cursor
                          {:default-mode :billing-orders
                           :browse-modes [{:mode-id   :billing-orders
                                           :mode-view browser/search-widget}
                                          {:mode-id :billing-balance}]})
     (when (#{:billing-orders} browse-mode)
       (browser/cursored-browser
         module cursor
         {:item-renderer       order-reader
          :item-cursor-prepare order-item-cursor-prepare
          :query-defaults      {:limit         50
                                :offset        0
                                :search-string ""
                                :browse-mode   browse-mode
                                :target        :server}
          :query               {:qn :action.adm/billing-browse-orders}}))

     (when (#{:billing-balance} browse-mode)
       (balance-manager module cursor))
     ]))

(defn check-item-permission [adm item result-chan]
  (let [{:keys [core session-reader]} adm]
    (async/pipeline
      1
      result-chan
      (comp
        (filter (fn [response] (get-in response [:result :permitted?])))
        (map (fn [_] item)))
      (api/call-async
        core
        {:qn      :authorizer/authorize-event
         :msg     (:event item)
         :user-id (get-principal session-reader :db/id)})
      )))

(defn get-permitted-items [adm]
  (let [menu-items (menu-items-key adm [])
        ch-out (async/chan 1 (filter some?))
        ch-in (async/to-chan menu-items)]
    (async/pipeline-async 1 ch-out (partial check-item-permission adm) ch-in)
    (async/into [] ch-out)))

(defn register-menu-items [adm]
  (go
    (let [{:keys [spa session-reader]} adm
          menu-items (<! (get-permitted-items adm))
          menu-group-ref (menu-group-key adm)
          menu-group [{:menu-item/type :menu-divider}
                      {:menu-item/id    :app.adm/adm
                       :menu-item/type  :menu-folder
                       :spa/menu-source menu-items}]]
      (if (and (authenticated? session-reader) (seq menu-items))
        (if-let [menu-group-id @menu-group-ref]
          (api/register-root-menu-group spa menu-group menu-group-id)
          (reset! menu-group-ref (api/register-root-menu-group spa menu-group)))
        (when-let [menu-group-id @menu-group-ref]
          (api/unregister-root-menu-group spa menu-group-id)
          (reset! menu-group-ref nil))))))

(defn handle-session-updated [adm _]
  (register-menu-items adm))

(defrecord Adm []
  api/IModule
  api/IComponent
  (start [adm]
    (register-menu-items adm)
    adm)
  (stop [adm]
    adm))

(defn new-adm [& [m]]
  (map->Adm
    (merge
      {api/id-key          :adm
       api/static-deps-key [:uri-builder :localizer :core :spa :session-reader]
       api/event-subs-key  [{:msg-filter :session/updated
                             :handler    handle-session-updated}]
       menu-items-key      [{:menu-item/id   :app.adm/users
                             :menu-item/type :menu-item
                             :event          {:qn :action.adm/users}}
                            {:menu-item/id   :app.adm/billing
                             :menu-item/type :menu-item
                             :event          {:qn :action.adm/billing}}
                            {:menu-item/id   :app.adm/hubs
                             :menu-item/type :menu-item
                             :event          {:qn :action.adm/hubs}}
                            {:menu-item/id   :app.helpdesk/helpdesk-adm
                             :menu-item/type :menu-item
                             :event          {:qn :action.helpdesk/admin-dashboard}}]
       api/routes-key      [{:msg-filter      :action.adm/users
                             :route-component users-dashboard}
                            {:msg-filter       :action.adm/update-user
                             :route-component  users-dashboard
                             :route-merge-data {:value :loading}}
                            {:msg-filter      :action.adm/hubs
                             :route-component hubs-dashboard}
                            {:msg-filter       :action.adm/update-hub
                             :route-component  hubs-dashboard
                             :route-merge-data {:value :loading}}
                            {:msg-filter      :action.adm/billing
                             :route-component billing-dashboard}]
       menu-group-key      (atom nil)}
      m)))
