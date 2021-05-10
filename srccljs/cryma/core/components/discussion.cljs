(ns cryma.core.components.discussion
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [clojure.string :as clostr]
    [cryma.core.api :as api]
    [cryma.core.tools :as tools]
    [cryma.core.tools-ui :as tools-ui]
    [cryma.core.components.common :as common]
    [rum.core :as rum]
    [taoensso.timbre :refer-macros (log trace debug info warn)]
    [cljs.core.async :as async :refer [<! put! >! alts!]]
    [cryma.core.components.html :as html]
    [cryma.core.components.rte.rte-full :as rtef]
    [cryma.core.components.rtext.user :as rteu]
    [cryma.core.components.rtext.extensions :as rte-ext]
    [cryma.core.components.cursors :as c]
    [cryma.core.components.channels :as ch]
    [cryma.core.components.delayed :as d]
    [cryma.core.components.ui-events :as uiev]
    [cryma.core.dtl.core :as dtl]
    [cryma.core.dtl.transformers.comparison :as comparison]))

(defn comment>key [{:keys [:db/id :sys/anchor]}]
  (if (nil? id)
    (str "new" (:db/id anchor))
    id))

(defn sort-branch [opts branch]
  (vec (sort-by (:sort-field opts) < branch)))

(defn comment-pred [child-predicate-rule child]
  (dtl/transform child child-predicate-rule comparison/transformer))

(defn make-d8n-tree
  "Transforms d8n tree according to supplied opts:
  max-levels - maximum number of nesting levels, starting from 1 for the root branch;
  child-field - field keyword to find descendants;
  sort-field - field keyword to sort descendants in resulting branch.
  Can take root branch of d8n, or particular item and starting level.
  Recursively visits all descendants, until recursion level is less then max-levels, if so -
  forms vector of all deeper descendants sorted by sort-field ascending."
  ([opts items]
   (->>
     items
     (filter (partial comment-pred (:child-predicate-rule opts)))
     (mapv (partial make-d8n-tree opts 1))))
  ([{:keys [max-levels child-field sort-field child-predicate-rule] :as opts} level item]
   (assert max-levels "max-levels required for make-d8n-tree")
   (assert child-field "child-field required for make-d8n-tree")
   (assert sort-field "sort-field required for make-d8n-tree")
   (letfn [(update-child-field [item]
             (update item child-field
                     (fn [childs]
                       (vec (remove (partial comment-pred child-predicate-rule) childs)))))]
     (if-let [childs (seq (filter
                            (partial comment-pred child-predicate-rule)
                            (child-field item)))]
       (if (< (inc level) max-levels)
         (->
           item
           (assoc
             :childs
             (->>
               childs
               (map
                 (fn [ch] (make-d8n-tree opts (inc level) ch)))
               (sort-branch opts)))
           update-child-field)
         (let [childs
               (mapcat
                 (fn [ch] (concat
                            [(update-child-field ch)]
                            (:childs
                              (make-d8n-tree opts (inc level) ch)))) childs)]
           (-> item
               (assoc :childs (sort-branch opts childs))
               update-child-field)))
       (assoc item :childs [])))))

(defn id->branch-index [branch dbid]
  (->>
    branch
    (map-indexed (fn [i item] [i item]))
    (filter (fn [[_ item]] (= (:db/id item) dbid)))
    first))

(defn tree-path->levels-path [tree path]
  (loop [branch [tree] path path result [] level 0]
    (if-let [k (first path)]
      (let [[i item] (id->branch-index branch k)
            childs? (:childs item)]
        (recur (or childs? branch)
               (rest path)
               (if childs?
                 (conj (conj result i) :childs)
                 result)
               (inc level)))
      (vec (rest result)))))

(defn -last-if-new? [branch]
  (when-let [last (-> branch last)]
    (when (-> last c/cval :db/id nil?) last)))

(defn -relative-pos [base-path chk]
  (let [c1 (count base-path)
        c2 (count chk)]
    (cond
      (= base-path chk)
      :equal
      (< c1 c2)
      :before
      (> c1 c2)
      :after
      :else
      (let [to-take c1
            base-path (remove #{:childs} (take to-take base-path))
            chk (remove #{:childs} (take to-take chk))]
        (loop [base-path base-path chk chk]
          (let [b (first base-path)
                c (first chk)]
            (if (and b c)
              (cond
                (< b c) :after
                (> b c) :before
                :else (recur (rest base-path) (rest chk)))
              :equal)))))))

(defn pass-editor-cursor? [editor-cursor branch-or-comment]
  (when (and
          editor-cursor branch-or-comment
          (c/child-path?
            (c/-path branch-or-comment)
            (:editor-path @editor-cursor)))
    (c/actual editor-cursor)))

(defn should-render-comment-branch? [comment editor]
  (let [childs (:childs comment)]
    (or (seq childs)
        (and comment editor (c/child-path?
                              (c/-path comment)
                              (:editor-path @editor))))))

(defn editor-branch-index [branch-path editor-path]
  (let [branch-count (count branch-path)
        editor-count (count editor-path)]
    (when (and branch-path editor-path (< branch-count editor-count))
      (first (subvec editor-path branch-count editor-count)))))

(defn short-show-count [branch editor short-show]
  (let [editor-path (when editor (:editor-path @editor))
        branch-path (when branch (c/-path branch))
        editor-index (editor-branch-index branch-path editor-path)
        branch-count (count branch)]
    (if editor-index
      (max (- branch-count editor-index) short-show)
      short-show)))

(rum/defcs
  comment-branch
  < (c/cursored {:local {:short-show nil :collapsed? false}})
  (ch/channels)
  (c/cursored-chans [:rum/args 1])
  {:will-mount
   (fn [state]
     (when-let [local (c/actual (c/local state :local))]
       (let [level (or (tools-ui/get-opts state :level) 1)
             levels-opts (tools-ui/get-opts state :levels-opts)
             {:keys [short-show initial-collapse]}
             (or (get levels-opts level)
                 (get levels-opts :default))]
         (c/swp! local merge {:short-show short-show
                              :collapsed? (= initial-collapse :collapsed)})))
     state)}
  [state module branch-cursor editor-cursor
   {:keys [item-renderer discussion-nonwatch] :as opts}]
  (let [l (:localizer module)
        {:keys [short-show collapsed?]}
        (c/actual (c/local state :local))
        actual-branch (c/actual branch-cursor)
        actual-editor (when editor-cursor (c/actual editor-cursor))
        cnt (count actual-branch)
        short-show (short-show-count actual-branch actual-editor @short-show)
        has-more? (> cnt short-show)]
    [:ul.comments
     (concat
       (if has-more?
         [[:li
           {:key "branch-control"}
           [:span.branch-control
            {:on-click #(c/swp! collapsed? not)}
            (if @collapsed?
              (l {:code  :discussion/show-comments
                  :count (if (> cnt short-show) (- cnt short-show) cnt)})
              (l :discussion/hide-comments))]]]
         [])
       (map
         (fn [{:keys [childs] :as comment}]
           [:li.comment
            {:key (comment>key @comment)}
            (item-renderer
              module comment (pass-editor-cursor? actual-editor comment) opts)
            (when (should-render-comment-branch? comment actual-editor)
              [:div.childs
               (comment-branch module childs (pass-editor-cursor? actual-editor childs)
                               (update opts :level inc))])])
         (if (and @collapsed? has-more?)
           (vec (take-last short-show actual-branch))
           actual-branch)))
     (when (and actual-editor
                (= (c/-path actual-branch) (:editor-path @actual-editor)))
       (item-renderer module (:comment-cache actual-editor) nil opts))]))

(rum/defcs
  comment-item
  < (c/cursored
      {:local {:edition-permitted? false}}
      {:nonwatch {:delayed (d/delayed-ctor 200)}})
  (ch/channels)
  (c/cursored-chans
    [:rum/args 1]
    (fn [state ev]
      (let [comment (c/actual (c/cursor state 1))
            editor (c/cursor state 2)
            edit? (and editor (= (c/-path comment) (:editor-path @editor)))
            comment (if edit? (:comment-cache editor) comment)
            delayed @(:delayed (c/non-watchable state :nonwatch))
            ch4write (ch/ch4write state)]

        (case (:qn ev)

          :d8n.comment/change-focus
          (when-not (seq (:data-path ev))
            (when-not (:suppress-blink? ev)
              (ch/alts!ev ch4write
                          {:qn         :d8n.comment/blink
                           :blink-type :blink-types/focus})))

          :d8n.comment/change-error
          (when-not (seq (:data-path ev))
            (let [errors (:errors ev)]
              (c/swp! comment update :sys
                      (fn [sys] (-> sys
                                    (assoc :status (when (seq errors) :error))
                                    (assoc :errors errors))))
              (when (and (seq errors) (not (:suppress-blink? ev)))
                (ch/alts!ev ch4write
                            {:qn         :d8n.comment/blink
                             :blink-type :blink-types/error}))
              (ch/alts!ev ch4write
                          {:qn              :d8n.comment/change-focus
                           :focused?        true
                           :suppress-blink? true})))

          :d8n.comment/blink
          (when-not (seq (:data-path ev))
            (c/swp! comment assoc-in [:sys :blink?] (:blink-type ev))
            (d/delayed-fn-call delayed
                               (fn [] (c/swp! comment assoc-in [:sys :blink?] nil))))

          :d8n.comment/publish
          (when-not (seq (:data-path ev))
            (async/onto-chan
              ch4write
              [{:qn              :d8n.comment/change-focus
                :focused?        true
                :suppress-blink? true}
               {:qn                 :d8n/publish-comment
                :cursors/broadcast? true}]
              false))

          :on-key-down
          (when (= [] (:data-path ev))
            (let [{:keys [kk ctrl?]} ev]
              (cond
                (= kk :key/esc)
                (ch/alts!ev ch4write {:qn                 :d8n/cancel-editor
                                      :cursors/broadcast? true
                                      :suppress-checks?   true})
                (and (= kk :key/enter) (not ctrl?))
                (ch/alts!ev ch4write {:qn :d8n.comment/publish}))))
          nil))))
  {:will-mount
   (fn [state]
     (let [comment (c/cursor state 1)
           local (c/local state :local)
           authorize-change-query
           (assoc (tools-ui/get-opts state :authorize-change-query)
             :sys/author (-> @comment :sys/author :db/id))
           module (first (:rum/args state))
           core (:core module)]
       (when (:db/id @comment)
         (go
           (let [response (<! (api/call-async
                                core authorize-change-query))]
             (when (and (api/success? core response)
                        (get-in response [:result :permitted?]))
               (c/swp! local assoc :edition-permitted? true))))))
     state)}

  (common/mix-global-click
    (fn [state e]
      (let [node (tools-ui/get-node state)
            inside (.contains node (.-target e))]
        (when-not inside
          (ch/alts!stev state {:qn                 :d8n/cancel-editor
                               :cursors/broadcast? true}))))
    (fn [state]
      (let [{:keys [:db/id]} @(c/cursor state 1)]
        (and (nil? id)))))
  [state module comment editor opts]
  (let [l (:localizer module)
        edition-permitted? (:edition-permitted? @(c/local state :local))
        edit? (and editor (= (c/-path comment) (:editor-path @editor)))
        comment (if edit? (:comment-cache editor) comment)
        {:keys [:db/id
                :sys/date
                :sys/author
                :comment/body
                :sys/version
                :sys] :as actual} (c/actual comment)
        {:keys [blink? errors status]} @sys
        error? (= status :error)
        loading? (= status :loading)
        new? (nil? @id)
        {:keys [ratings-component ratings-component-opts]} opts]
    [:div.comment
     {:class (str
               (when blink? (str "blink-" (name blink?)))
               (when error? " error"))
      :ref   (when new? "newcomment")}
     [:div.comment-head
      (common/render-author-info module @author true)
      (when (not new?)
        (common/date-renderer
          @date
          {:l               l
           :today           true :yesterday true
           :just-now        60
           :since-minutes   59
           :since-hours     24
           :this-year       true
           :update-interval 60000}))
      (when (and (not new?) (not edit?)
                 (not= (:db/id @version) (:tx/origin-id @version)))
        [:span.changed-flag
         (l :discussion/comment-was-changed)])
      (when (or new? edit?)
        [:span.editor-title
         (l (if new? :discussion/comment-editor-title-new
                     :discussion/comment-editor-title-edit))])
      (when (and edition-permitted? (not new?) (not edit?))
        [:span.comment-edit
         {:title    (l :discussion/comment-edit-title)
          :on-click (fn [_] (ch/alts!stev state {:qn :d8n/edit}))}])]

     (when (and (not new?) (not edit?))
       (html/html->hiccup @body (partial rte-ext/build-post-tag module @actual)))

     (when (or new? edit?)
       (rtef/rte-full
         body
         (rte-ext/light-rte-opts
           module
           {:placeholder (l :discussion/comment-body-placeholder)
            :focused?    true
            :disabled?   loading?
            :key-down-spec
                         {:filter
                          (fn [ev]
                            (or (= (:kk ev) :key/esc)
                                (and (= (:kk ev) :key/enter)
                                     (not (:ctrl? ev)))))
                          :handler
                          (fn [ev]
                            (ch/alts!stev
                              state (assoc ev :data-path [:comment/body]))
                            :cancel-event)}})))
     [:div.control
      (when (and (not new?) (not edit?) ratings-component)
        (ratings-component
          module
          actual
          ratings-component-opts))
      (when (and (not new?) (not edit?))
        [:span.answer
         {:on-click (fn [e]
                      (.stopPropagation e)
                      (ch/alts!stev state {:qn :d8n/answer}))}
         [:span (l :discussion/answer)]])

      (when (and error? (or new? edit?))
        (common/render-errors module errors))
      [:span.edit-btns
       (when (and (or new? edit?) loading?)
         [:span.loading
          (l :discussion/sending)])
       (when (or new? edit?)
         [:span.edit-publish.btn
          {:class    (str
                       (when loading? " disabled"))
           :on-click #(when-not loading?
                       (ch/alts!stev state {:qn :d8n.comment/publish}))}
          (l (if new? :discussion/publish-comment-new
                      :discussion/publish-comment-edit))])
       (when (or new? edit?)
         [:span.edit-cancel.btn
          {:class    (str
                       (when loading? " disabled"))
           :on-click (fn [_]
                       (when-not loading?
                         (ch/alts!stev
                           state
                           {:qn                 :d8n/cancel-editor
                            :cursors/broadcast? true
                            :suppress-checks?   true})))}
          (l :discussion/cancel-comment)])]]]))

(def username-re #"^\s*\@[а-яА-ЯёЁ\w\d_-]+\s*,?\s*")

(defn get-answer-branch [src-cursor]
  (let [{:keys [childs] :as src} src-cursor]
    (if (nil? @childs)
      (c/parent-cursor src 1)
      childs)))

(defn username-node-predicate [node]
  (and (= :a (:tag node))
       (->> node :content clostr/join string?)
       (->> node :content clostr/join (re-find username-re))))

(defn -starts-with-username? [s]
  (html/starts-with? (str s) username-node-predicate))

(defn -replace-username? [module s new-username]
  (html/edit-first-node
    (str s)
    username-node-predicate
    (fn [node]
      (when (seq new-username)
        (->
          node
          (assoc-in [:attrs :href] (rteu/user-uri module new-username))
          (assoc :content [(str "@" new-username)]))))))

(defn empty-answer-body [username]
  (str
    "<html><head></head><body data-keks=\"{:type :block, :semantics :document}\"><p data-keks=\"{:type :line, :semantics :generic-line}\"><a data-keks=\"{:type :material, :semantics :user}\">@"
    username
    "</a><span data-keks=\"{:type :material, :semantics :text}\"> </span></p></body></html>"))

(defn -empty-body? [comment]
  (and comment
       (->
         comment
         c/cval
         :comment/body
         (html/empty-html? username-node-predicate))))

(defn -answer-body [module answer-source template]
  (let [old-body (:comment/body template)
        answer-to (get-in answer-source [:sys/author :user/nickname])]
    (cond
      (and (html/empty-html? old-body) answer-to)
      (empty-answer-body answer-to)
      (-starts-with-username? old-body)
      (-replace-username? module old-body answer-to)
      :else
      (str old-body))))

(defn -prepare-answer [module answer-source template]
  (->
    template
    (assoc :comment/body (-answer-body module answer-source template))
    (assoc :sys/anchor (:db/id answer-source))))

(defn -prepare-publish [comment & [edit?]]
  (tools/prepare-value
    (-> @comment
        (update :comment/body html/trim)
        (dissoc :childs :sys :sys/version :sys/entity :sys/date :sys/_anchor :sys/author)
        (tools/->if edit? dissoc :sys/anchor))))

(defn -levels-count [path]
  (count (filter #{:childs} path)))

(defn -add-childs [{:keys [max-levels]} path v]
  (if (< (-levels-count path) max-levels)
    (assoc v :childs [])
    v))

(defn node-height [state]
  (let [node (tools-ui/get-node state)
        rect (.getBoundingClientRect node)]
    (- (.-bottom rect) (.-top rect))))

(rum/defcs
  discussion
  ;; :editor/:editor-path serves as pointer to currently opened editor
  ;; for new comments it contains the branch path, and that branch creates fake comment at the bottom
  ;; for existing comments (editing) it contains the comment path
  ;; editor always rendered by comment-renderer
  < (c/cursored {:tree   {}
                 :editor {:editor-path   nil
                          :comment-cache nil}}
                {:nonwatch
                 {:notif-unsub nil}})
  (ch/channels :new-root-chan!)
  (c/cursored-chans
    (c/local-path :tree)
    (fn [state ev]
      (let [{:keys [tree-query
                    subscription-query
                    publish-new-query
                    authorize-new-query
                    publish-change-query
                    comment-template] :as opts}
            (tools-ui/get-opts state)
            ch4write (ch/ch4write state)
            module (first (:rum/args state))
            core (:core module)
            parent-data (c/cursor state 1)
            tree (c/actual (c/local state :tree))
            editor (c/actual (c/local state :editor))
            nonwatch (c/actual (c/non-watchable state :nonwatch))]

        (case (:qn ev)
          :d8n/start-query-tree
          (when tree-query
            (c/rst! tree :loading)
            (async/pipeline
              1
              ch4write
              (map (fn [res] {:qn       :d8n/finish-query-tree
                              :response res}))
              (api/call-async core tree-query) false))

          :d8n/finish-query-tree
          (let [{:keys [result errors] :as response} (:response ev)
                new-val (if (api/success? core response)
                          (make-d8n-tree
                            opts 0
                            (assoc @parent-data :sys/_anchor result))
                          [:error errors])]
            (c/rst! tree new-val)
            (when (api/success? core response)
              (ch/alts!ev ch4write {:qn :d8n/query-subscribe})))

          :d8n/query-subscribe
          (when (and subscription-query (nil? (:notif-unsub @nonwatch)))
            (let [notif-ch (api/call-async core subscription-query)]
              (go
                (let [result (<! notif-ch)
                      notif-unsub (get-in result [:result :unsub-ch])]
                  (when (api/success? core result)
                    (c/swp! nonwatch assoc :notif-unsub notif-unsub)
                    (async/pipe notif-ch ch4write false))))))

          :sys.channels/closing
          (when-not (seq (:data-path ev))
            (when-let [unsub (:notif-unsub @nonwatch)]
              (async/close! unsub)
              (c/swp! nonwatch dissoc :notif-unsub)))

          :d8n/edit
          (ch/alts!ev ch4write (assoc ev :qn :d8n/permitted-edit))

          :d8n/permitted-edit
          (let []
            (let [src (:src-cursor ev)]
              (c/rst! editor {:editor-path   (:data-path ev)
                              :comment-cache @src})))

          :d8n/answer
          (go
            (let [response (<! (api/call-async core authorize-new-query))]
              (if (get-in response [:result :permitted?])
                (ch/alts!ev ch4write (assoc ev :qn :d8n/permitted-answer))
                (api/pub-event core {:qn         :authorization/permission-denied
                                     :denied-msg publish-new-query}))))

          :d8n/permitted-answer
          (let [src (:src-cursor ev)
                answer-branch (get-answer-branch src)
                new-branch-path (c/-path answer-branch)
                new-answer (-prepare-answer
                             module @src
                             (assoc
                               (or (and (nil? (get-in @editor [:comment-cache :db/id]))
                                        (:comment-cache @editor))
                                   comment-template)
                               :sys/author
                               (:sys/author comment-template)))]
            (c/swp! editor (fn [nw]
                             (-> nw
                                 (assoc :editor-path new-branch-path)
                                 (assoc :comment-cache new-answer))))
            (ch/alts!ev ch4write
                        {:qn              :d8n.comment/change-focus
                         :focused?        true
                         :suppress-blink? (not= (:editor-path @editor) new-branch-path)
                         :target-child?   [:comment-cache]}))

          :d8n/cancel-editor
          (when (:editor-path @editor)
            (when (or (:suppress-checks? ev)
                      (-empty-body? (:comment-cache editor)))
              (c/rst! editor {})))

          :d8n/publish-comment
          (let [real-src (c/actual (:src-cursor ev))
                edit? (not (nil? (:db/id @real-src)))
                src (c/swp! (if edit? (:comment-cache editor) real-src)
                            (fn [src] (->
                                        src
                                        (assoc-in [:sys :errors] [])
                                        (assoc-in [:sys :status] :loading))))
                prepared (-prepare-publish src edit?)
                result-qn (if edit?
                            :d8n/finish-publish-comment-change
                            :d8n/finish-publish-comment)]
            (if (-empty-body? prepared)
              (ch/alts!ev ch4write
                          {:qn     result-qn
                           :src    src
                           :result (api/error-response
                                     core {}
                                     {:field
                                      :comment/body
                                      :code
                                      :constraint.violation.string/not-empty})})
              (async/pipeline
                1
                ch4write
                (map (fn [res] {:qn     result-qn
                                :src    real-src
                                :result res}))
                (api/call-async core
                                (assoc (if edit?
                                         publish-change-query
                                         publish-new-query) :value prepared)) false)))

          :d8n/finish-publish-comment-change
          (let [{:keys [src result]} ev
                {:keys [result errors] :as response} result
                editor-path (:editor-path @editor)]
            (cond
              (api/success? core response)
              (let [result (-add-childs opts editor-path result)]
                (c/swp! nonwatch assoc :last-event-path editor-path)
                (c/rst! editor {})
                (c/swp! src merge result))
              (api/error? core response)
              (ch/alts!ev ch4write
                          {:qn            :d8n.comment/change-error
                           :target-child? editor-path
                           :errors        errors})))

          :d8n/finish-publish-comment
          (let [{:keys [src result]} ev
                {:keys [result errors] :as response} result]
            (cond
              (api/success? core response)
              (let [editor-path (:editor-path @editor)
                    result (-add-childs opts editor-path result)]
                (c/swp! nonwatch assoc :last-event-path editor-path)
                (c/swp! editor update
                        :comment-cache #(->
                                         %
                                         (assoc :sys {})
                                         (assoc :comment/body "")))
                (c/swp! (get-in tree editor-path) conj result))
              (api/error? core response)
              (ch/alts!ev ch4write
                          {:qn            :d8n.comment/change-error
                           :target-child? src
                           :cursors/broadcast? true
                           :errors        errors})))

          :notifications.events/entity-added
          (let [branch-path (tree-path->levels-path @tree (pop (:anchor-path ev)))
                new-comment (-add-childs opts branch-path (:entity ev))
                comment-branch (get-in tree branch-path)
                new-id (:db/id new-comment)]
            (when-not
              (id->branch-index @comment-branch new-id)
              (c/swp! nonwatch assoc :last-event-path branch-path)
              (c/swp! comment-branch
                      (fn [branch]
                        (sort-branch opts (conj branch new-comment))))))

          :notifications.events/entity-changed
          (let [branch-path (tree-path->levels-path @tree (pop (:anchor-path ev)))
                {:keys [:db/id] :as changed-comment} (:entity ev)
                comment-branch (get-in tree branch-path)]
            (when-let
              [[i _] (id->branch-index @comment-branch id)]
              (c/swp! nonwatch assoc :last-event-path
                      (conj branch-path i))
              (c/swp! comment-branch update i merge changed-comment)))
          nil))))
  {:will-mount
   (fn [state]
     (ch/alts!stev state {:qn :d8n/start-query-tree})
     state)
   :will-update
   (fn [state]
     (let [nonwatch (c/actual (c/non-watchable state :nonwatch))]
       (c/swp! nonwatch assoc :height-before (node-height state)))
     state)
   :did-update
   (fn [state]
     (let [{:keys [height-before last-event-path]}
           @(c/actual (c/non-watchable state :nonwatch))
           editor-path (:editor-path @(c/local state :editor))
           new-height (node-height state)
           wa (.getElementById js/document "working-area")
           pos (-relative-pos editor-path last-event-path)]
       (when (and editor-path (#{:equal :before} pos))
         (aset wa "scrollTop" (+ (.-scrollTop wa)
                                 (- new-height height-before)))))
     state)}

  [state module parent-data opts]
  (let [l (:localizer module)
        tree (c/actual (c/local state :tree))
        editor (c/actual (c/local state :editor))
        error? (and (vector? @tree) (= :error (first @tree)))]
    [:div.discussion
     (cond
       (= :loading @tree)
       [:div.loading (l :loading)]
       error?
       (if-let [error-renderer (:error-renderer opts)]
         (error-renderer module (second @tree))
         [:div.error (l (get-in @tree [1 0] :error))])
       :else
       (let [childs (:childs tree)]
         [:div
          [:ul.discuss-head]
          (comment-branch
            module childs
            (pass-editor-cursor? editor childs)
            (merge opts
                   {:item-renderer comment-item
                    :levels-opts   {1        {:short-show       5
                                              :initial-collapse :collapsed}
                                    :default {:short-show       3
                                              :initial-collapse :collapsed}}}))

          (when (not= [:childs] (:editor-path @editor))
            [:span.fake-input
             {:key      "fake"
              :on-click (fn [e]
                          (.stopPropagation e)
                          (ch/alts!stev
                            state
                            {:qn :d8n/answer}))}
             (l :discussion/comment-body-placeholder)])]))]))















