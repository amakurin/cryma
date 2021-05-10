(ns cryma.app.helpdesk
  (:require [cryma.core.api :as api]
            [cryma.app.db :as db]
            [datomic.api :as d]
            [cryma.core.tools :as tools]))

(defn auth-result [request & ks]
  (let [auth-result (:auth-result (meta request))]
    (if (seq ks)
      (get-in auth-result ks)
      auth-result)))

(defn filtered-db [data-provider filt-ctors]
  (api/get-db-filtered
    data-provider
    (filterv some? filt-ctors)))

(defn get-comments [db request & [with-info?]]
  (db/fetch-data
    db
    (-> request
        (update
          :pattern
          #(or %
               [:*
                {:sys/author [:db/id :user/nickname]}
                {:sys/entity [:db/id :db/ident]}
                {:sys/_anchor
                 [:*
                  {:sys/author [:db/id :user/nickname]}
                  {:sys/entity [:db/id :db/ident]}
                  {:sys/_anchor '...}]}]))
        (assoc :entity :entity.helpdesk/comment))
    with-info?))

(defn get-comment [db request]
  (first (get-comments db request)))

(defn write-comment
  [conn user-id comment]
  (let [comment (db/prepare-entity-data
                  (d/db conn) comment :entity.helpdesk/comment user-id)
        tx-result @(d/transact conn
                               [[:app/add-entity comment user-id]])
        dbid (db/get-tx-id comment tx-result)]
    (get-comment
      (:db-after tx-result)
      {:filter {:db/id dbid}})))

(defn edit-comment
  [conn user-id comment]
  (let [comment (-> comment
                    (select-keys [:db/id :comment/body])
                    tools/prepare-value)
        tx-result @(d/transact conn [[:app/update-entity comment user-id]])
        dbid (db/get-tx-id comment tx-result)]
    (get-comment
      (:db-after tx-result)
      {:filter {:db/id dbid}})))

(defn get-support-requests [db request & [with-info?]]
  (db/fetch-data
    db
    (-> request
        (update
          :pattern
          #(or %
               [:*
                {:sys/author [:db/id :user/nickname]}
                {:sys/entity [:db/id :db/ident]}
                {:sys/status [:db/id :db/ident]}
                {:support-request/category [:db/id :db/ident]}
                {:support-request/severity [:db/id :db/ident]}]))
        (update
          :search-fields
          #(or %
               [:support-request/title
                :support-request/body]))
        (update
          :order
          #(or %
               [:sys/date :db/id]))
        (assoc :entity :entity.helpdesk/support-request))
    with-info?))

(defn get-support-request [db request]
  (first (get-support-requests db request)))

(defn prepare-support-request [db user-id support-request operation]
  (let [support-request (-> support-request
                            (tools/->if (= :create operation)
                                        (fn [sr] (if (:sys/status sr)
                                                   sr (assoc sr :sys/status :support-request-statuses/open))))
                            (select-keys
                              [:db/id :sys/status
                               :support-request/title :support-request/body
                               :support-request/category :support-request/severity :sys/description]))]
    (if (= :create operation)
      (db/prepare-entity-data db support-request :entity.helpdesk/support-request user-id)
      (tools/prepare-value support-request))))

(defn upsert-support-request
  [conn user-id support-request operation & [dry?]]
  (let [db (d/db conn)
        support-request (prepare-support-request db user-id support-request operation)
        tx-fn (case operation
                :create :app/add-entity
                :update :app/update-entity)
        tx-result (if dry?
                    (d/with db [[:app/add-entity support-request user-id]])
                    @(d/transact conn [[tx-fn support-request user-id]]))
        dbid (db/get-tx-id support-request tx-result)]
    (get-support-request (:db-after tx-result)
                         {:filter {:db/id dbid}})))

(defn serve-simple-search
  [entity module request]
  (let [{:keys [core data-provider]} module
        params
        (merge
          (select-keys
            request [:search-string :filter :pattern :order :limit :offset])
          {:entity        entity
           :search-fields (:search-fields request [:db/doc])})]
    (api/respond-success
      core request (db/fetch-data (api/get-db data-provider) params))))

(defn serve-category-search
  [module request]
  (serve-simple-search :entity.helpdesk/support-request-category module request))

(defn serve-severity-search
  [module request]
  (serve-simple-search :entity.helpdesk/support-request-severity module request))

(defn serve-status-search
  [module request]
  (serve-simple-search :entity.helpdesk/support-request-status module request))

(defn serve-create-request [helpdesk request]
  (let [{:keys [core data-provider]} helpdesk]
    (api/respond-success
      core request
      (upsert-support-request
        (api/get-conn data-provider)
        (auth-result request :user-id)
        (:value request)
        :create
        (:dry-run? request)))))

(defn serve-read-request [helpdesk request]
  (let [{:keys [core data-provider]} helpdesk
        permitted-action (auth-result request :permitted-action :db/ident)
        db (filtered-db data-provider
                        [(when (not= :action.helpdesk/read-request permitted-action)
                           (partial db/exclude-attr-filter-ctor :sys/description))])
        result (assoc-in (get-support-request
                           db
                           (if (= permitted-action :action.helpdesk/customer-read-request)
                             (assoc-in request [:filter :sys/author] (auth-result request :user-id))
                             request))
                         [:auth-result :permitted-action]
                         permitted-action)]
    (api/respond-success core request result)))

(defn serve-read-request-discussion
  [module request]
  (let [{:keys [core data-provider]} module
        db (filtered-db data-provider [])]
    (api/respond-success
      core request
      (get-comments db request))))

(defn serve-update-request [helpdesk request]
  (let [{:keys [core data-provider]} helpdesk
        current-user-id (auth-result request :user-id)
        permitted-action (auth-result request :permitted-action :db/ident)
        conn (api/get-conn data-provider)
        value
        (-> request
            :value
            (tools/->if
              (= permitted-action :action.helpdesk/customer-update-request)
              select-keys
              [:db/id
               :support-request/title
               :support-request/body
               :support-request/severity
               :support-request/category
               :sys/status]))]
    (api/respond-success
      core request
      (upsert-support-request
        conn
        current-user-id
        value
        :update
        (:dry-run? request)))))

(defn serve-write-comment
  [module request]
  (let [{:keys [core data-provider]} module
        result (write-comment
                 (api/get-conn data-provider)
                 (auth-result request :user-id)
                 (:value request))]
    (api/respond-success core request result)
    (api/pub-event
      core
      {:qn          :notifications.events/entity-added
       :entity      result
       :anchor-path (db/anchor-path (api/get-db data-provider) (:db/id result))
       :target      :notifier/subscribers})))

(defn serve-edit-comment
  [module request]
  (let [{:keys [core data-provider]} module
        result (edit-comment
                 (api/get-conn data-provider)
                 (auth-result request :user-id)
                 (:value request))]
    (api/respond-success core request result)
    (api/pub-event
      core
      {:qn          :notifications.events/entity-changed
       :entity      result
       :anchor-path (db/anchor-path (api/get-db data-provider) (:db/id result))
       :target      :notifier/subscribers})))

(defn serve-browse-requests [helpdesk request]
  (let [{:keys [core data-provider]} helpdesk
        permitted-action (auth-result request :permitted-action :db/ident)]
    (api/respond-success
      core request
      (get-support-requests
        (filtered-db data-provider [])
        (->
          request
          (tools/->if
            (= permitted-action :action.helpdesk/customer-browse-requests)
            assoc-in [:filter :sys/author] (auth-result request :user-id)))
        {:value-field :value}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DECLARATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn ring-handler [app request]
  (let [handler (:spa-handler app)]
    (handler request)))

(defn action-browse-dtls [& [deault-mode]]
  [:dtlrules
   [:browse-mode [{:p [:params :browse-mode] :or (or deault-mode :all)}]]
   [:limit [[:params :limit]]]
   [:offset [[:params :offset]]]
   [:search-string [[:params :search-string]]]])

(def action-dbid-dtls
  [:dtlrules
   [:db/id [[:params :id]]]])

(def uri-mode-dtl
  ["%s/" [:browse-mode]])

(def uri-browse-dtls
  [:dtlrules
   uri-mode-dtl
   ["%s" [:dtlrules {:? "?" :& "&"}
          ["limit=%s" [:limit]]
          ["offset=%s" [:offset]]
          ["search-string=%s"
           [:search-string]]]]])

(defn routes [& route-descs]
  (mapv
    (fn [route-desc]
      (let [spec (peek route-desc)]
        (conj (pop route-desc) (assoc spec :handler ring-handler))))
    route-descs))

(defrecord Helpdesk []
  api/IModule
  api/IComponent
  (start [helpdesk]
    helpdesk)
  (stop [helpdesk]
    helpdesk))

(def create-request-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value
     [:map/spec-closed
      {:support-request/title    db/non-empty-string-required-shema
       :support-request/body     db/non-empty-string-required-shema
       :support-request/category db/id-required-schema
       :support-request/severity db/id-required-schema
       }]}]])

(def update-request-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value
     [:map/spec-closed
      {:db/id                    db/id-required-schema
       :support-request/title    db/non-empty-string-shema
       :support-request/body     db/non-empty-string-shema
       :support-request/category db/id-or-ident-schema
       :support-request/severity db/id-or-ident-schema
       :sys/description          db/non-empty-string-shema
       :sys/status               db/id-or-ident-schema
       }]}]])

(def write-comment-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value [:map/spec-closed
             {:comment/body db/non-empty-string-required-shema
              :sys/anchor   db/id-required-schema}]}]])

(def edit-comment-schema
  [[:data/type :data-type/map]
   [:map/spec
    {:value [:map/spec-closed
             {:db/id        db/id-required-schema
              :comment/body db/non-empty-string-required-shema}]}]])

(defn new-helpdesk [& [m]]
  (map->Helpdesk
    (merge
      {api/id-key :helpdesk
       api/static-deps-key
                  [:data-provider :spa-handler]
       api/configuration-key
                  {}
       api/request-servers-key
                  [{:msg-filter      :helpdesk/category-search
                    :handler         #'serve-category-search
                    :validation-opts {:schema db/search-schema}}
                   {:msg-filter      :helpdesk/severity-search
                    :handler         #'serve-severity-search
                    :validation-opts {:schema db/search-schema}}
                   {:msg-filter      :helpdesk/status-search
                    :handler         #'serve-status-search
                    :validation-opts {:schema db/search-schema}}

                   {:msg-filter         :action.helpdesk/create-request
                    :handler            #'serve-create-request
                    :validation-opts    {:schema create-request-schema}
                    :authorization-opts {:category-search-root :access-category/helpdesk}}

                   {:msg-filter         :action.helpdesk/read-request
                    :handler            #'serve-read-request
                    :validation-opts    {:schema db/read-schema}
                    :authorization-opts {:category-search-root :access-category/helpdesk
                                         :rule-opts            {:brule.app/authorship
                                                                {:rule-root [:filter :db/id]}}}}

                   {:msg-filter         :action.helpdesk/read-request-discussion
                    :handler            #'serve-read-request-discussion
                    :authorization-opts {:check-permission     :action.helpdesk/read-request
                                         :category-search-root :access-category/helpdesk
                                         :rule-opts            {:brule.app/authorship
                                                                {:rule-root [:filter :sys/anchor]}}}}

                   {:msg-filter         :action.helpdesk/update-request
                    :handler            #'serve-update-request
                    :validation-opts    {:schema update-request-schema}
                    :authorization-opts {:category-search-root :access-category/helpdesk
                                         :rule-opts            {:brule.app/authorship
                                                                {:rule-root [:value :db/id]}}}}

                   {:msg-filter         :action.helpdesk/write-comment
                    :handler            #'serve-write-comment
                    :validation-opts    {:schema write-comment-schema
                                         :scope  #{:client}}
                    :authorization-opts {:check-permission     :action.helpdesk/read-request
                                         :category-search-root :access-category/helpdesk
                                         :rule-opts            {:brule.app/authorship
                                                                {:rule-root [:value :sys/anchor]}}}}

                   {:msg-filter         :action.helpdesk/edit-comment
                    :handler            #'serve-edit-comment
                    :validation-opts    {:schema edit-comment-schema
                                         :scope  #{:client}}
                    :authorization-opts {:check-permission     :action.helpdesk/read-request
                                         :category-search-root :access-category/helpdesk
                                         :rule-opts            {:brule.app/authorship
                                                                {:rule-root [:value :db/id]}}}}

                   {:msg-filter         :action.helpdesk/browse-requests
                    :handler            #'serve-browse-requests
                    :validation-opts    {:schema db/search-schema}
                    :authorization-opts {:category-search-root :access-category/helpdesk}}

                   {:msg-filter         :action.helpdesk/customer-browse-requests
                    :handler            #'serve-browse-requests
                    :validation-opts    {:schema db/search-schema}
                    :authorization-opts {:category-search-root :access-category/helpdesk}}]
       api/routes-key
                  (routes
                    ["/helpdesk/" :get ""
                     {:action-qn   :action.helpdesk/customer-dashboard
                      :action-rule (action-browse-dtls)
                      :uri-rule    ["/helpdesk/%s" uri-browse-dtls]}]
                    ["/helpdesk/" :get [[keyword :browse-mode] "/"]
                     {:action-qn   :action.helpdesk/customer-dashboard
                      :action-rule (action-browse-dtls :search)
                      :uri-rule    ["/helpdesk/%s" uri-browse-dtls]}]
                    ["/helpdesk-adm/" :get ""
                     {:action-qn   :action.helpdesk/admin-dashboard
                      :action-rule (action-browse-dtls :search)
                      :uri-rule    ["/helpdesk-adm/%s" uri-browse-dtls]}]
                    ["/helpdesk-adm/" :get [[keyword :browse-mode] "/"]
                     {:action-qn   :action.helpdesk/admin-dashboard
                      :action-rule (action-browse-dtls :search)
                      :uri-rule    ["/helpdesk-adm/%s" uri-browse-dtls]}]
                    ["/support-requests/" :get [[long :id] "/"]
                     {:action-qn   :action.helpdesk/read-request
                      :action-rule [:filter action-dbid-dtls]
                      :uri-rule    ["/support-requests/%s/" [[:filter :db/id]]]}]
                    ["/support-requests/" :get [[long :id] "/edit/"]
                     {:action-qn   :action.helpdesk/update-request
                      :action-rule [:filter action-dbid-dtls]
                      :uri-rule    ["/support-requests/%s/edit/" [[:filter :db/id]]]}])}
      m)))
