(ns cryma.core.components.select
  (:require-macros
    [cljs.core.async.macros :refer [go go-loop]])
  (:require [cryma.core.components.common :as common]
            [cryma.core.components.delayed :as d]
            [cryma.core.api :as api]
            [rum.core :as rum]
            [cljs.core.async :refer [<! put! >! alts!]]
            [cryma.core.tools-ui :as tools-ui]
            [taoensso.timbre :refer-macros (log trace debug info warn)]
            [cryma.core.components.cursors :as c]
            [cryma.core.components.channels :as ch]
            [cryma.core.components.ui-events :as uiev]
            [cljs.core.async :as async]))

(rum/defcs
  selected-item
  < (ch/channels)
    (c/cursored)
    (c/cursored-chans [:rum/args 0])
  [state cursor {:keys [item-renderer] :as opts}]
  (let [item-key-field (or (:item-key-field opts) :db/id)
        actual (c/actual cursor)]
    [:li.item
     {:class (str
               (when (nil? (item-key-field @actual))
                 " new"))}
     (item-renderer @actual)
     [:span.delete
      {:on-click
       (fn [e]
         ;(.stopPropagation e)
         (.preventDefault e)
         (ch/alts!stev
           state
           {:qn                 :delete-selected-item
            :item               @actual
            :cursors/broadcast? true}))}]]))

(rum/defcs
  drop-down-list
  < (c/cursored)
    (ch/channels)
    (c/cursored-chans [:rum/args 1])
  [state module cursor {:keys [item-renderer] :as opts}]
  (assert item-renderer "Drop down list needs item-renderer")
  (let [l (:localizer module)
        actual (c/actual cursor)
        {:keys [value index]} @actual
        error? (and (vector? value) (= :error (first value)))]
    [:div.drop-down
     {:ref   "drop-down"
      :class (when (or (not (vector? value))
                       (empty? value)
                       error?) "no-data")}
     (cond
       (= :loading value)
       [:div.loading (l :loading)]
       error?
       [:div.error (l (get-in value [1 0] :error))]
       (empty? value)
       [:div.nothing-found (l :nothing-found)]
       :else
       [:ul
        (map-indexed
          (fn [i item]
            [:li
             {:key            i
              :class          (when (= i index) "current")
              :on-click       (fn [e]
                                ;(.stopPropagation e)
                                (.preventDefault e)
                                (ch/alts!stev state {:qn   :select-source-item
                                                     :item item}))
              :on-mouse-enter #(c/swp! actual assoc :index i)}
             (item-renderer item)]) value)])]))

(defn change-index [op {:keys [value] :as selection}]
  (if (and (vector? value) (seq value))
    (update selection :index (fn [i] (mod (op i) (count value))))
    (assoc selection :index 0)))

(defn enough-length? [search-string opts]
  (>= (count @search-string)
      (:min-search-string-length opts 0)))

(defn focus-search-input [state]
  (when-let [search (tools-ui/get-node state "searchInput")]
    (.focus search)))

(rum/defcs
  cursored-multi-select
  < (ch/channels :new-root-channel!)
    (c/cursored {:local {:selection-source-visible? false
                         :selection-source          {:value :loading :index 0}
                         :focus                     false
                         :to-delete                 nil
                         :caller                    (d/delayed-ctor 500)}
                 :query {:search-string ""}})
    {:will-mount
     (fn [state]
       (c/rst! (c/local state :query)
               (tools-ui/get-opts state :query-template))
       state)}
    (c/cursored-chans
      (c/local-path :local)
      (fn [state ev]
        #_(println "------------------SELECT rum/id::" (:rum/id state)
                 (:caller (c/actual (c/local state :local)))
                 " event::" ev)
        (let [module (first (:rum/args state))
              core (:core module)
              opts (tools-ui/get-opts state)
              {:keys
               [selection-source-visible?
                selection-source
                to-delete
                focus
                caller] :as local}
              (c/actual (c/local state :local))
              {:keys [search-string] :as actual-query}
              (c/actual (c/local state :query))
              actual-value (c/actual (second (:rum/args state)))
              ch4write (ch/ch4write state)]
          (case (:qn ev)

            :start-query-source
            (when (enough-length? search-string opts)
              (c/rst! selection-source {:value :loading :index 0})
              (async/pipeline
                1
                ch4write
                (map (fn [res] {:qn     :finish-query-source
                                :result res}))
                (if (:delayed-query? ev)
                  (d/delayed-async-call @caller core @actual-query)
                  (api/call-async core @actual-query)) false))

            :finish-query-source
            (when @selection-source-visible?
              (let [{:keys [result errors] :as response} (:result ev)
                    new-val (if (api/success? core response)
                              (vec (remove (set @actual-value)
                                           result))
                              [:error errors])]
                (c/rst! selection-source {:value new-val :index 0})))

            :show-selection-source
            (when (and (not @selection-source-visible?)
                       (enough-length? search-string opts))
              (async/onto-chan ch4write
                               [{:qn :cancel-deletion}
                                {:qn             :start-query-source
                                 :delayed-query? (:delayed-query? ev)}]
                               false)
              (c/rst! selection-source-visible? true))

            :hide-selection-source
            (when @selection-source-visible?
              (c/rst! selection-source-visible? false))

            :inc-selection-index
            (when @selection-source-visible?
              (c/swp! selection-source (partial change-index inc)))

            :dec-selection-index
            (when @selection-source-visible?
              (c/swp! selection-source (partial change-index dec)))

            :select-source-item
            (when-let [selected (:item ev)]
              (c/swp!
                actual-value
                (fn [val]
                  (let [val (if (vector? val) val [])
                        max-selected-count (:max-selected-count opts 20)]
                    (if (some #(= % selected) val)
                      val
                      (conj
                        (if (< (count val) max-selected-count)
                          val
                          (vec (butlast val)))
                        selected)))))
              (ch/alts!ev ch4write {:qn :cancel-selection}))

            :change-search
            (let [{:keys [search suppress-query? delayed-query?]} ev]
              (c/rst! search-string search)
              (when-not suppress-query?
                (ch/alts!ev ch4write {:qn             :start-query-source
                                      :delayed-query? delayed-query?}))
              (focus-search-input state))

            :change-focus
            (let [{:keys [focused? suppress-query?]} ev]
              (c/rst! focus focused?)
              (when focused? (focus-search-input state))
              (if focused?
                (when-not suppress-query?
                  (ch/alts!ev ch4write {:qn :show-selection-source}))
                (ch/alts!ev ch4write {:qn :hide-selection-source})))

            :cancel-selection
            (async/onto-chan ch4write
                             [{:qn :change-search :search "" :suppress-query? true}
                              {:qn :hide-selection-source}]
                             false)

            :cancel-deletion
            (when @to-delete
              (c/swp! local dissoc :to-delete))

            :delete-selected-item
            (let [item (or (:item ev) @to-delete)]
              (if item
                (do
                  (ch/alts!ev ch4write {:qn :cancel-deletion})
                  (c/swp!
                    actual-value
                    (fn [items] (->> items (remove #(= % item)) vec))))
                (when (seq actual-value)
                  (c/swp!
                    local assoc :to-delete (last @actual-value))))
              (focus-search-input state))

            :on-key-down
            (let [{:keys [kk]} ev
                  add-new-item-on-keyset (:add-new-item-on-keyset opts)]
              (when-not (= :key/backspace kk)
                (ch/alts!ev ch4write {:qn :cancel-deletion}))
              (cond
                (and add-new-item-on-keyset (add-new-item-on-keyset kk))
                (when (enough-length? search-string opts)
                  (ch/alts!ev ch4write
                              {:qn   :select-source-item
                               :item {(:add-new-item-field opts) (.trim @search-string)}}))
                :else
                (case kk
                  :key/enter
                  (if @selection-source-visible?
                    (when-let [item (get-in @selection-source
                                            [:value (:index @selection-source)])]
                      (ch/alts!ev ch4write
                                  {:qn   :select-source-item
                                   :item item}))
                    (ch/alts!ev ch4write {:qn :show-selection-source}))

                  :key/down-arrow
                  (if @selection-source-visible?
                    (ch/alts!ev ch4write {:qn :inc-selection-index})
                    (ch/alts!ev ch4write {:qn :show-selection-source}))

                  :key/up-arrow
                  (if @selection-source-visible?
                    (ch/alts!ev ch4write {:qn :dec-selection-index})
                    (ch/alts!ev ch4write {:qn :show-selection-source}))
                  :key/esc
                  (ch/alts!ev ch4write
                              {:qn :cancel-selection})

                  :key/tab
                  (ch/alts!ev ch4write
                              {:qn       :change-focus
                               :focused? false})

                  :key/backspace
                  (when (and (nil? (seq @search-string))
                             (seq actual-value))
                    (ch/alts!ev ch4write
                                {:qn :delete-selected-item}))
                  (ch/alts!ev ch4write
                              {:qn             :show-selection-source
                               :delayed-query? true}))))
            nil))))

    (common/mix-global-click->focus-event)

  [state module value-cursor
   {:keys [placeholder class item-key-field item-renderer
           add-new-item-on-keyset] :as opts}]
  (assert item-renderer
          "item-renderer is required for multi-item-select")
  (let [local (c/actual (c/local state :local))
        {:keys [search-string]} (c/actual (c/local state :query))
        actual-value (c/actual value-cursor)
        drop-down-list (or (:drop-down-list opts) drop-down-list)
        {:keys [selection-source-visible? focus to-delete]} local
        has-value? (or (seq @actual-value) (seq (str @search-string)))]
    [:div.multi-item-select
     {:class (str class
                  (when has-value? " with-value")
                  (when @focus " focus")
                  (when @selection-source-visible? " dropped")
                  (when @to-delete " del"))}
     [:span.input-text
      (when has-value? [:span.ph placeholder])
      [:ul
       (map-indexed
         (fn [i item]
           (rum/with-key
             (selected-item
               item
               {:item-key-field item-key-field
                :item-renderer  item-renderer})
             i)) actual-value)
       [:li
        [:input
         {:placeholder  (when-not has-value? placeholder)
          :ref          "searchInput"
          :type         "text"
          :on-focus     #(ch/alts!stev
                          state {:qn              :change-focus
                                 :focused?        true
                                 :suppress-query? true})
          :on-key-down  #(let [ev (uiev/wrap-event % :on-key-down)]
                          (when-not ((set add-new-item-on-keyset)
                                      (:kk ev))
                            (ch/alts!stev state ev)
                            (when (#{:key/up-arrow :key/down-arrow} (:kk ev))
                              (.preventDefault %))))
          :on-key-press #(let [ev (uiev/wrap-event % :on-key-down)]
                          (when ((set add-new-item-on-keyset)
                                  (:kk ev))
                            (ch/alts!stev state ev)
                            (.preventDefault %)))
          :on-change    #(ch/alts!stev
                          state {:qn             :change-search
                                 :search         (.. % -target -value)
                                 :delayed-query? true})
          :value        @search-string
          :size         (when has-value? (max 5 (count @search-string)))
          :disabled     false
          :maxLength    50
          }]]]]
     (when @selection-source-visible?
       [:div.drop-down-wrapper
        (drop-down-list
          module
          (:selection-source local)
          {:item-renderer (:drop-down-item-renderer opts)})])]))
