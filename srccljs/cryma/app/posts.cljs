(ns cryma.app.posts
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cryma.core.api :as api]
    [cryma.core.tools-ui :as tools-ui]
    [cryma.core.tools :as tools]
    [cryma.core.components.common :as common]
    [cryma.core.components.browser :as browser]
    [cryma.core.components.select :as select]
    [cryma.core.components.input :as input]
    [cryma.core.components.html :as html]
    [cryma.core.components.rte.rte-full :as rte-full]
    [cryma.core.components.rtext.extensions :as rte-ext]
    [cryma.core.components.discussion :as disscus]
    [cryma.app.promo :as promo]
    [cryma.app.anchored :as anchored]
    [rum.core :as rum]
    [cljs.core.async :as async :refer [<! put! >! alts!]]
    [cljs.reader :as r]
    [cryma.core.components.cursors :as c]
    [cryma.core.api :as api]
    [cryma.core.components.channels :as ch]
    [cryma.core.schema :as sch]))

(defn post-item-cursor-prepare [item-cursor]
  (c/cur
    {:qn               :action.app/read-post
     :filter           {:db/id (:db/id @item-cursor)}
     :sys/cached-value @item-cursor}))

(defn render-meta-coll [module coll
                        {:keys [ul-class li-class
                                item-event-map-fn link-title-key link-text-key]}]
  (when (seq coll)
    [:ul
     {:class ul-class}
     (->> coll
          (map-indexed
            (fn [i item]
              [:li
               {:key   i
                :class li-class}
               (common/event-link
                 module
                 (assoc (item-event-map-fn item)
                   :sys/cached-value item)
                 (if link-title-key {:title (link-title-key item)} {})
                 (link-text-key item))]))
          (interpose [:li.comma ", "]))]))

(rum/defcs
  browser-cut
  < (c/cursored)
  (ch/channels)
  (c/cursored-chans
    [:rum/args 1]
    (fn [state ev]
      (let [reader-local (second (rest (:rum/args state)))]
        (case (:qn ev)
          :post-reader/finish-query-value
          (c/swp! reader-local dissoc :undercut-loading?)
          nil))))
  [state module full-cursor reader-local cut-content]
  (let [l (:localizer module)
        collapsed? (:undercut-collapsed? @reader-local)
        loading? (:undercut-loading? @reader-local)]
    [:span
     {:on-click (fn [_]
                  (if collapsed?
                    (do
                      (c/swp! reader-local assoc :undercut-collapsed? false :undercut-loading? true)
                      (ch/alts!stev state {:qn :post-reader/query-value
                                           :undercut? true}))
                    (c/swp! reader-local assoc :undercut-collapsed? true :undercut-loading? false)))}
     [:div.cut
      (if (or collapsed? loading?)
        cut-content
        [:p [:span
             (l :app.post/cut-collapse)]])
      (when loading? [:span.loading (l :loading)])]]))

(defn build-post-tag [module full-cursor reader-local browse? tag attrs childs]
  (if (and (= tag :a) (= (:id attrs) "cut"))
    (when browse?
      (browser-cut module full-cursor reader-local (first childs)))
    (rte-ext/build-post-tag module @full-cursor tag attrs childs)))

(rum/defcs
  post-reader
  <
  (c/cursored
    {:local {:permitted-action    :action.app/read-post
             :undercut-collapsed? true
             :edition-permitted?  false}}
    {:nonwatch
     {:notif-unsub nil}})
  (ch/channels)
  common/mix-scroll-to
  (c/cursored-chans
    [:rum/args 1]
    (fn [state ev]
      (let [module (first (:rum/args state))
            core (:core module)
            cursor (second (:rum/args state))
            nonwatch (c/actual (c/non-watchable state :nonwatch))
            ch4write (ch/ch4write state)
            mode (tools-ui/get-opts state :mode)
            preview?
            (#{:item-renderer.modes/browser-embedded
               :item-renderer.modes/preview} mode)
            support-interactivity? (not= :item-renderer.modes/preview mode)
            value-attr (if preview? :sys/cached-value :value)
            local (c/local state :local)]
        (case (:qn ev)

          :post-reader/mount
          (if (and (not preview?) (nil? (:value @cursor)))
            (ch/alts!ev ch4write {:qn :post-reader/query-value})
            (do
              (ch/alts!ev ch4write {:qn :post-reader/authorize-edition})
              (ch/alts!ev ch4write {:qn :post-reader/query-subscribe})))

          :post-reader/authorize-edition
          (when support-interactivity?
            (go
              (let [response
                    (<! (api/call-async
                          core {:qn                 :authorizer/check-permission
                                :action-id          :action.app/edit-post
                                :access-category-id (get-in @cursor [value-attr :sys/access-category :db/ident])
                                :sys/author         (get-in @cursor [value-attr :sys/author :db/id])}))]
                (when (and (api/success? core response)
                           (get-in response [:result :permitted?]))
                  (c/swp! local assoc :edition-permitted? true)))))

          :post-reader/query-value
          (do
            (when-not (:undercut? ev)
              (c/swp! cursor assoc value-attr :loading))
            (async/pipeline
              1
              ch4write
              (map (fn [response] {:qn       :post-reader/finish-query-value
                                   :response response}))
              (api/call-async core (assoc (common/event->request @cursor)
                                     :qn :action.app/read-post
                                     :target :server)) false))

          :post-reader/finish-query-value
          (let [response (:response ev)]
            (if (api/success? core response)
              (let [result (:result response)]
                (c/swp! local
                        (fn [local]
                          (-> local
                              (update
                                :permitted-action
                                (fn [pa]
                                  (or (get-in result [:auth-result :permitted-action])
                                      pa)))
                              (assoc :scroll-to (:scroll-to @cursor)))))
                (c/swp! cursor assoc value-attr (dissoc result :auth-result))
                (ch/alts!ev ch4write {:qn :post-reader/authorize-edition})
                (ch/alts!ev ch4write {:qn :post-reader/query-subscribe}))
              (c/swp! cursor assoc value-attr [:error (:errors response)])))

          :post-reader/query-subscribe
          (when (and support-interactivity? (nil? (:notif-unsub @nonwatch)))
            (let [id (get-in @cursor [value-attr :db/id])
                  notif-ch
                  (api/call-async
                    core
                    {:qn      :notifier/subscribe
                     :filters [[:dtlrules
                                [:notifications.events/entity-changed [:qn]]
                                [:entity.app/post [[:entity :sys/entity :db/ident]]]
                                [id [[:anchor-path 0]]]]]})]
              (go
                (let [result (<! notif-ch)
                      notif-unsub (get-in result [:result :unsub-ch])]
                  (when (api/success? core result)
                    (c/swp! nonwatch assoc :notif-unsub notif-unsub)
                    (async/pipe notif-ch ch4write false))))))

          :notifications.events/entity-changed
          (c/swp! cursor update value-attr merge (:entity ev))

          :sys.channels/closing
          (when-not (seq (:data-path ev))
            (when-let [unsub (:notif-unsub @nonwatch)]
              (async/close! unsub)
              (c/swp! nonwatch dissoc :notif-unsub)))

          nil))))

  {:will-mount
   (fn [state]
     (ch/alts!stev state {:qn :post-reader/mount})
     state)}
  [state module cursor {:keys [mode] :as opts}]
  (let [{:keys [localizer session-reader]} module
        local (c/local state :local)
        permitted-action (:permitted-action @local)
        edition-permitted? (:edition-permitted? @local)
        browse? (= mode :item-renderer.modes/browser-embedded)
        preview? (#{:item-renderer.modes/browser-embedded
                    :item-renderer.modes/preview} mode)
        show-full-body? (or (not browse?) (not (:undercut-collapsed? @local)))
        support-interactivity? (not= :item-renderer.modes/preview mode)
        actual (c/actual cursor)
        src (if preview?
              (:sys/cached-value actual)
              (:value actual))
        {:keys [:db/id
                :post/title
                :sys/access-category
                :sys/date
                :app/comments
                :sys/author
                :post/hub
                :post/tag
                :post/preview
                :post/undercut] :as value} @src
        error? (and (vector? value) (= :error (first value)))]
    (cond
      (or (= :loading value) (nil? value))
      [:div.page [:div.loading (localizer :loading)]]
      error?
      [:div.page [:div.error (localizer (get-in value [1 0] :error))]]
      value
      [:div.page.post
       {:class (str
                 (when preview? " preview")
                 (when browse? " browse"))}
       [:h1.title
        [:span.title-head
         (common/date-renderer
           date
           {:l               localizer
            :today           true :yesterday true
            :just-now        100 :since-minutes 59
            :since-hours 24
            :this-year       true
            :update-interval 120000})
         (when edition-permitted?
           (common/event-link module {:qn     :action.app/edit-post
                                      :filter {:db/id id}}
                              {:class "edit-link"
                               :title (localizer :post-reader/edit-link-title)}
                              (localizer :post-reader/edit-link-title)))]
        (if browse?
          (common/event-link module @actual {}
                             [:span.post-title title])
          [:span.post-title title])
        (let [ac (:db/ident access-category)]
          [:span.access {:class (when ac (name ac)) :title (localizer ac)}])

        (render-meta-coll
          module
          hub
          {:ul-class       "hubs"
           :li-class       "hub"
           :item-event-map-fn
                           (fn [hub]
                             {:qn     :action.app/read-hub
                              :filter {:db/id (:db/id hub)}})
           :link-title-key :hub/description
           :link-text-key  :hub/name})]
       (->
         (if show-full-body?
           (html/join-documents preview (or undercut ""))
           preview)
         (html/html->hiccup (partial build-post-tag module actual local browse?)))

       (when (and (= permitted-action :action.app/read-post-preview)
                  show-full-body?)
         (promo/embedded-promo
           (:promo module)
           @actual
           {:entity          :entity.app/post
            :action          :action.app/read-post
            :access-category (:db/ident access-category)}))

       (render-meta-coll
         module
         tag
         {:ul-class          "tags"
          :li-class          "tag"
          :item-event-map-fn (fn [tag]
                               {:qn            :action.app/browse-posts
                                :browse-mode   :search
                                :search-string (:tag/title tag)})
          :link-text-key     :tag/title})
       [:ul.footer
        [:li.ratings
         (anchored/ratings module src
                           {:authorize-rate-query
                                      {:qn                 :authorizer/check-permission
                                       :action-id          :action.app/rate-post
                                       :access-category-id (:db/ident access-category)}
                            :rate-query
                                      {:qn     :action.app/rate-post
                                       :value  {:sys/anchor nil :rate/value nil}
                                       :target :server}
                            :enabled? support-interactivity?})]
        [:li.general
         [:span.views
          [:span.view-count (:post/views @src "-")]]
         (anchored/favorities module src
                              {:authorize-favorite-query
                                         {:qn                 :authorizer/check-permission
                                          :action-id          :action.app/favorite-post
                                          :access-category-id (:db/ident access-category)}
                               :favorite-query
                                         {:qn     :action.app/favorite-post
                                          :value  {:sys/anchor nil}
                                          :target :server}
                               :enabled? support-interactivity?})
         (common/render-author-info module author)]
        [:li.comments
         [:span.comments
          {:on-click (fn [_]
                       (when support-interactivity?
                         (if browse?
                           (api/pub-event
                             (:core module)
                             (assoc
                               @actual

                               :scroll-to {:scroll-to "discussion"
                                                        :type      :scroll.type/center}))
                           (c/swp! local assoc :scroll-to {:scroll-to "discussion"
                                                           :type      :scroll.type/center}))))}
          [:span.comment-count
           (or comments 0)]]]]
       [:div {:ref "discussion"}]
       (when-not preview?
         (disscus/discussion
           module
           src
           {:max-levels
            2

            :tree-query
            {:qn     :action.app/read-post-discussion
             :filter {:sys/anchor id}
             :target :server}

            :subscription-query
            {:qn      :notifier/subscribe
             :filters [[:dtlrules
                        [:notifications.events/entity-added [:qn]]
                        [:entity.app/comment [[:entity :sys/entity :db/ident]]]
                        [id [[:anchor-path 0]]]]
                       [:dtlrules
                        [:notifications.events/entity-changed [:qn]]
                        [:entity.app/comment [[:entity :sys/entity :db/ident]]]
                        [id [[:anchor-path 0]]]]]}
            :publish-new-query
            {:qn     :action.app/write-comment
             :value  nil
             :target :server}

            :authorize-new-query
            {:qn                 :authorizer/check-permission
             :action-id          :action.app/write-comment
             :access-category-id (:db/ident access-category)}

            :publish-change-query
            {:qn     :action.app/edit-comment
             :value  nil
             :target :server}

            :authorize-change-query
            {:qn                 :authorizer/check-permission
             :action-id          :action.app/edit-comment
             :access-category-id (:db/ident access-category)
             :sys/author         nil}

            :comment-template
            {:sys/author   (api/read-val session-reader [:principal :db/id])
             :comment/body ""
             :sys/anchor   id}

            :ratings-component
            anchored/ratings
            :ratings-component-opts
            {:authorize-rate-query
             {:qn                 :authorizer/check-permission
              :action-id          :action.app/rate-comment
              :access-category-id (:db/ident access-category)}
             :rate-query
             {:qn     :action.app/rate-comment
              :value  {:sys/anchor nil :rate/value nil}
              :target :server}
             :enabled?
             support-interactivity?}

            :error-renderer
            (fn [module errors]
              (let [error (first errors)]
                (if (= (:code error) :authorization/permission-denied)
                  (promo/embedded-promo
                    (:promo module) errors
                    {:entity          :entity.app/post
                     :action          :action.app/read-post-discussion
                     :access-category (:db/ident access-category)})
                  [:div.error (localizer error)])))
            :key-field
            :db/id

            :child-field
            :sys/_anchor

            :child-predicate-rule
            [:entity.app/comment [[:sys/entity :db/ident]]]

            :sort-field
            :sys/date}))
       ])))

(rum/defcs
  feed-panel
  < (c/cursored)
  [state module cursor]
  (let [{:keys [localizer]} module]
    [:div.feed-panel
     (common/event-link
       module
       {:qn :action.app/browse-hubs}
       {:class "caption hubs"}
       (localizer :post/feed-hub-settings))
     (common/event-link
       module
       {:qn :action.app/browse-user-profiles}
       {:class "caption authors"}
       (localizer :post/feed-author-settings))]))

(rum/defcs
  post-browser
  < (c/cursored)
  [state module cursor]
  (let [{:keys [localizer]} module
        cursor (c/actual cursor)
        browse-mode (:browse-mode @cursor)]
    [:div.page
     [:h1 (localizer :app/posts)]
     (common/browser-head module cursor
                   {:default-mode :all
                    :browse-modes [{:mode-id :all}
                                   {:mode-id   :feed
                                    :mode-view feed-panel}
                                   {:mode-id :best}
                                   {:mode-id   :search
                                    :mode-view browser/search-widget}]})
     (browser/cursored-browser
       module cursor
       {:item-renderer       post-reader
        :item-cursor-prepare post-item-cursor-prepare
        :query-defaults      {:limit         10
                              :offset        0
                              :search-string ""
                              :browse-mode browse-mode
                              :target        :server}})]))

(defn split-post-body [post]
  (let [{:keys [before after]}
        (html/split-document (or (:post/body post) "")
                             (fn [node]
                               (-> node
                                   (get-in [:attrs :data-keks] "")
                                   (r/read-string)
                                   :semantics #{:cut})))
        before (if (html/empty-html? before) "" before)
        after (if (html/empty-html? after) "" after)]
    (-> post
        (dissoc :post/body)
        (assoc :post/preview before)
        (assoc :post/undercut after))))

(defn prepare-errors [errors]
  (->>
    errors
    (mapcat (fn [err]
              (if (= :value (:field err))
                (sch/flatten-map-errors [(-> err
                                             (assoc :post/body (:post/preview err))
                                             (dissoc :field))])
                [err])))
    vec))

(def post-keys
  [:db/id
   :post/title
   :sys/access-category
   :post/hub
   :post/preview
   :post/undercut
   :post/tag])

(defn upsert-post-request [cursor & [dry?]]
  (-> @cursor
      (update :value
              (fn [post]
                (-> post
                    (update :sys/access-category first)
                    (update :post/body html/trim)
                    split-post-body
                    (select-keys post-keys)
                    tools/prepare-value)))
      (tools/->if dry? assoc :dry-run? true)
      (dissoc :sys/guid)
      (assoc :target :server)))

(rum/defcs
  post-editor
  < (c/cursored {:local {}})
  (ch/channels)
  common/mix-scroll-to
  (c/cursored-chans
    [:rum/args 1]
    (fn [state ev]
      (let [module (first (:rum/args state))
            {:keys [core localizer]} module
            cursor (second (:rum/args state))
            ch4write (ch/ch4write state)]
        (case (:qn ev)

          :post-editor/mount
          (when (= :action.app/edit-post (:qn @cursor))
            (ch/alts!ev ch4write {:qn :post-editor/query-value}))

          :post-editor/query-value
          (do
            (c/swp! cursor assoc :value :loading)
            (async/pipeline
              1
              ch4write
              (map (fn [response] {:qn       :post-editor/finish-query-value
                                   :response response}))
              (api/call-async
                core
                (->
                  (common/event->request @cursor)
                  (dissoc :sys/guid :value)
                  (assoc
                    :qn :action.app/read-post
                    :disable-counters? true
                    :target :server)))
              false))

          :post-editor/authorize-edition
          (go
            (let [value (:value ev)
                  response
                  (<! (api/call-async
                        core {:qn                 :authorizer/check-permission
                              :action-id          :action.app/edit-post
                              :access-category-id (get-in value [:sys/access-category :db/ident])
                              :sys/author         (get-in value [:sys/author :db/id])}))]
              (if (and (api/success? core response)
                       (get-in response [:result :permitted?]))
                (c/swp! cursor assoc :value
                        (-> value
                            (select-keys post-keys)
                            (update :sys/access-category
                                    (fn [v] (if v
                                              [(assoc v :access-category/title
                                                        (localizer (:db/ident v)))]
                                              [])))
                            (assoc :post/body (html/join-documents
                                                (or (:post/preview value) "")
                                                (or (:post/undercut value) "")))
                            (dissoc :post/preview :post/undercut)))
                (let [errors (if (seq (:errors response))
                               (:errors response)
                               [{:code       :authorization/permission-denied
                                 :denied-msg {:qn :action.app/edit-post}}])]
                  (c/swp! cursor assoc :value [:error errors])))))

          :post-editor/finish-query-value
          (let [response (:response ev)]
            (if (api/success? core response)
              (ch/alts!ev ch4write {:qn    :post-editor/authorize-edition
                                    :value (:result response)})
              (c/swp! cursor assoc :value [:error (:errors response)])))

          nil))))
  {:will-mount
   (fn [state]
     (ch/alts!stev state {:qn :post-editor/mount})
     state)}
  [state module cursor]
  (let [{:keys [localizer core]} module
        local (c/local state :local)
        actual (c/actual cursor)
        value (:value actual)
        error? (and (vector? @value) (= :error (first @value)))
        loading? (or (= :loading @value) (nil? value) (nil? @value))]
    (cond
      loading?
      [:div.page [:div.loading (localizer :loading)]]
      error?
      [:div.page [:div.error (localizer (get-in @value [1 0] :error))]]
      value
      [:div.page
       [:ul.form
        {:ref "edit"}

        (common/wrap-frow
          module :post/title local
          (input/cursored-input
            module
            (:post/title value)
            {:placeholder (localizer :post/title)}))

        (common/wrap-frow
          module :sys/access-category local
          (select/cursored-multi-select
            module
            (:sys/access-category value)
            {:drop-down-item-renderer (fn [item]
                                        [:span.cat-item.access
                                         {:class (-> item :db/ident name)}
                                         [:span.name (:access-category/title item)]])
             :max-selected-count      1
             :item-renderer           (fn [item] [:span
                                                  {:class (str "access "
                                                               (-> item :db/ident name))}
                                                  (:access-category/title item)])
             :placeholder             (localizer :sys/access-category)
             :query-template          {:qn            :authorizer/get-permitted-categories
                                       :action-id     :action.app/create-post
                                       :search-string ""}}))

        (common/wrap-frow
          module :post/hub local
          (select/cursored-multi-select
            module
            (:post/hub value)
            {:drop-down-item-renderer (fn [item]
                                        [:span.hub-item.access
                                         {:class (-> item :sys/access-category :db/ident name)}
                                         [:span.name (:hub/name item)]
                                         [:span.description (:hub/description item)]])
             :max-selected-count      3
             :item-renderer           (fn [item] [:span
                                                  {:class (str "access "
                                                               (-> item :sys/access-category
                                                                   :db/ident name))}
                                                  (:hub/name item)])
             :placeholder             (localizer (if (:db/id @value) :post/hub :post/hub-new))
             :query-template          {:qn            :app/hub-search
                                       :search-string ""
                                       :order         [:hub/name]
                                       :limit         50
                                       :pattern       [:db/id :hub/name
                                                       :hub/description
                                                       {:sys/access-category
                                                        [:db/id :db/ident]}]
                                       :target        :server}}))

        (common/wrap-frow
          module :post/body local
          (rte-full/rte-full
            (:post/body value)
            (rte-ext/full-rte-opts module (localizer :post/body))))

        (common/wrap-frow
          module :post/tag local
          (select/cursored-multi-select
            module
            (:post/tag value)
            {:drop-down-item-renderer  (fn [item] [:span.tag-item
                                                   [:span.name (:tag/title item)]])
             :min-search-string-length 3
             :max-selected-count       10
             :item-renderer            :tag/title
             :add-new-item-field       :tag/title
             :add-new-item-on-keyset   #{:key/comma}
             :placeholder              (localizer (if (:db/id @value) :post/tag :post/tag-new))
             :query-template           {:qn            :app/tag-search
                                        :search-string ""
                                        :order         [:tag/title]
                                        :limit         20
                                        :pattern       [:db/id :tag/title]
                                        :target        :server}}))

        [:li.frow
         [:span.btn
          {:class "preview"
           :on-click
                  (fn [_]
                    (c/swp! local dissoc :preview :errors)
                    (go
                      (let [response
                            (<! (api/call-async
                                  core
                                  (upsert-post-request actual :dry-run!)))]
                        (if (api/success? core response)
                          (c/swp! local merge {:preview   (:result response)
                                               :scroll-to "preview"})
                          (c/swp! local assoc :errors
                                  (prepare-errors (:errors response)))))))}
          (localizer :post-editor/preview)]
         (common/render-errors module (filter #(-> % :field nil?) (:errors @local)))]

        (when (:preview @local)
          [:li.frow
           {:ref "preview"}
           (post-reader
             module
             (c/cur {:sys/cached-value (:preview @local)})
             {:mode :item-renderer.modes/preview})])
        (when (:preview @local)
          [:li.frow
           [:ul.control-btns
            [:li [:span.btn
                  {:class    "back-to-edit"
                   :on-click #(c/swp! local assoc :scroll-to "edit")}
                  (localizer :post-editor/back-to-edit)]]
            [:li [:span.btn
                  {:class
                   "publish"
                   :on-click
                   (fn [_]
                     (go
                       (let [response (<! (api/call-async
                                            core
                                            (upsert-post-request actual)))]
                         (when (api/success? core response)
                           (api/pub-event core {:qn     :action.app/read-post
                                                :filter {:db/id (get-in response [:result :db/id])}})
                           (c/swp! local assoc :errors
                                   (prepare-errors (:errors response)))))))}
                  (localizer :post-editor/publish)]]]])
        ]])))

(defrecord PostsModule []
  api/IModule
  api/IComponent
  (start [module] module)
  (stop [module] module))

(defn new-posts-module [& [m]]
  (map->PostsModule
    (merge
      {api/id-key          :posts-module
       api/static-deps-key [:localizer :core :uri-builder :promo :session-reader]
       api/routes-key      [{:msg-filter      :action.app/browse-posts
                             :route-component post-browser}
                            {:msg-filter      :action.app/read-post
                             :route-component post-reader}
                            {:msg-filter       :action.app/create-post
                             :route-component  post-editor
                             :route-merge-data {:value {}}}
                            {:msg-filter       :action.app/edit-post
                             :route-component  post-editor
                             :route-merge-data {:value :loading}}]}
      m)))

