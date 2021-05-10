(ns cryma.core.components.common
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require
    [cryma.core.api :as api]
    [cryma.core.tools-ui :as tools-ui]
    [cryma.core.tools :as tools]
    [rum.core :as rum]
    [cljs.core.async :as async :refer [<! put! >! alts!]]
    [goog.string :as gstring]
    goog.string.format
    [cryma.core.components.channels :as ch]
    [cryma.core.components.cursors :as c]))

(defn link-click [module event e]
  (.preventDefault e)
  (api/pub-event (:core module) event))

(defn prepare-event [ev]
  (dissoc ev :value :sys/guid))

(defn event-link [module event attrs content]
  [:a
   (-> attrs
       (assoc :href (tools-ui/event->route module event))
       (assoc :on-click (partial link-click module (prepare-event event))))
   content])

(defn event->request [ev]
  (dissoc (prepare-event ev) :sys/cached-value))

(defn mix-global-click [click-handler & [state-predicate]]
  (assert (fn? click-handler)
          "click-handler should be fn[state e] for mix-global-click")
  {:will-mount
   (fn [state]
     (let [state
           (assoc state
             :global-click (fn [e]
                             (when (or (nil? state-predicate)
                                       (state-predicate state))
                               (click-handler state e))))]
       (.addEventListener js/window "click" (:global-click state) false)
       state))
   :transfer-state
   (fn [old new]
     (assoc new :global-click (:global-click old)))
   :will-unmount
   (fn [state]
     (when-let [global-click (:global-click state)]
       (.removeEventListener js/window "click" global-click false))
     state)})

(defn mix-global-click->focus-event []
  (mix-global-click
    (fn [state e]
      (let [node (tools-ui/get-node state)
            inside (.contains node (.-target e))]
        (ch/alts!stev
          state
          {:qn       :change-focus
           :focused? inside})))))

(def mix-opts-should-update
  {:should-update
   (fn [old new]
     (not= (tools-ui/get-opts old)
           (tools-ui/get-opts new)))})

(defn scroll-to [{:keys [x y]}]
  (let [wa (.getElementById js/document "working-area")]
    (when x (aset wa "scrollLeft" x))
    (when y (aset wa "scrollTop" y))))

(def mix-scroll-to
  {:did-update
   (fn [state]
     (when-let [local (c/actual (c/local state :local))]
       (let [scroll (:scroll-to @local)
             {:keys [scroll-to condition cond-margin type]
              :or   {scroll-to   scroll
                     condition   :scroll.cond/always
                     cond-margin 0
                     type        :scroll.type/into}}
             (if (map? scroll) scroll {:scroll-to scroll})
             scroller
             (case
               type
               :scroll.type/into (fn [n] (.scrollIntoView n))
               :scroll.type/center
               (fn [n]
                 (let [rect (.getBoundingClientRect n)
                       win-height (.-innerHeight js/window)
                       top (.-top rect)
                       bottom (.-bottom rect)
                       wa (.getElementById js/document "working-area")
                       scroll-top (.-scrollTop wa)]
                   (aset wa "scrollTop"
                         (+ scroll-top
                            (- top (/ (- win-height
                                         (- bottom top)) 2))))))
               (throw (ex-info "Unsupported scroll type in mix-scroll-to"
                               {:type type})))
             predicate
             (case condition
               :scroll.cond/always (fn [_] true)
               :scroll.cond/partial-invisible
               (fn [n]
                 (let [rect (.getBoundingClientRect n)
                       top (.-top rect)
                       bottom (.-bottom rect)
                       win-height (.-innerHeight js/window)]
                   (or (< top cond-margin)
                       (> bottom (- win-height cond-margin)))))
               :scroll.cond/invisible
               (fn [n]
                 (let [rect (.getBoundingClientRect n)
                       top (.-top rect)
                       bottom (.-bottom rect)
                       win-height (.-innerHeight js/window)]
                   (and (> top (- win-height cond-margin))
                        (< bottom cond-margin))))
               (throw (ex-info "Unsupported scroll condition in mix-scroll-to"
                               {:condition condition})))]
         (when scroll-to
           (when-let [scroll-node (tools-ui/get-node state scroll-to)]
             (when (predicate scroll-node)
               (scroller scroll-node)))
           (c/swp! local dissoc :scroll-to))))
     state)})

(def mix-autoupdate
  {:did-mount
   (fn [state]
     (let [comp (:rum/react-component state)
           callback #(rum/request-render comp)
           interval (js/setInterval
                      callback
                      (+ (or
                           (tools-ui/get-opts state :update-interval)
                           1000)
                         (rand-int 1000)))]
       (assoc state ::interval interval)))
   :transfer-state
   (fn [old-state state]
     (merge state (select-keys old-state [::interval])))
   :will-unmount
   (fn [state] (js/clearInterval (::interval state)))})

(defn field>class [field]
  (let [ns (namespace field)]
    (str ns (when ns "-") (name field))))

(defn render-errors [module errors]
  (when (seq errors)
    (let [l (:localizer module)]
      (vec (concat [:ul.err-desc {:key "err"}]
                   (map (fn [err] [:li (l err)]) errors))))))

(defn wrap-frow [module field local view]
  (let [errors
        (->> @local :errors
             (filter #(= field (:field %))))]
    [:li.frow
     {:class (str (field>class field)
                  (when (seq errors) " error"))}
     view
     (render-errors module errors)]))

(rum/defc
  date-renderer
  < mix-autoupdate
  [data opts]
  [:span.date
   (:component-attrs opts {})
   (tools/date>str data opts)])

(defn render-author-info [module author & [no-prefix?]]
  [:span.author-info
   (when-not no-prefix?
     [:span "@"])
   (event-link
     module
     {:qn               :action.app/read-user-profile
      :filter           {:db/id (:db/id author)}
      :sys/cached-value author}
     {}
     (:user/nickname author))])

(rum/defcs
  browser-head
  < (c/cursored)
  [state module cursor opts]
  (let [{:keys [localizer]} module
        current-mode-id (or (:browse-mode @cursor) (:default-mode opts))
        browse-modes (:browse-modes opts)
        {:keys [mode-view cursor-path]}
        (when current-mode-id
          (some (fn [mode] (when (= current-mode-id (:mode-id mode)) mode))
                browse-modes))]
    [:div.head
     [:ul.switch
      (map (fn [{:keys [mode-id mode-event mode-class]}]
             [:li
              {:class (str (when mode-class mode-class)
                           (when (= current-mode-id mode-id) " active"))}
              (event-link
                module
                (or mode-event
                    (-> @cursor
                        (dissoc :limit :offset :search-string)
                        (assoc :browse-mode mode-id)))
                {:class (str "browse-mode-link " (when mode-class mode-class))}
                (localizer (keyword "browse-modes" (name mode-id))))])
           browse-modes)]
     (when mode-view
       (mode-view module
                  (if cursor-path
                    (get-in cursor cursor-path)
                    cursor)))]))


