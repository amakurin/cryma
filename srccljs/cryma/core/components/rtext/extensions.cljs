(ns cryma.core.components.rtext.extensions
  (:require
    [cryma.core.components.rte.rte-toolbox :as rtetb]
    [cryma.core.components.rtext.cut :as cut]
    [cryma.core.components.rtext.spoiler :as spoiler]
    [cryma.core.components.rtext.user :as user]
    [cryma.core.components.rte.model.tools :as mt]
    [cryma.core.components.rte.model.convertation :as mc]
    [hickory.render :as hr]
    [hickory.core :as hp]
    [hickory.zip :as hz]
    [clojure.zip :as z]
    [clojure.string :as clostr]
    [cryma.core.components.common :as common]
    [cryma.core.components.html :as html]
    #_[cryma.core.l10n :refer [l]]
    [cljs.reader :as r]
    [rum.core :as rum]))

;;=============================
;; TOOLS & CONF
;;=============================
(defn full-rte-concat-tools [module]
  [(user/user-tool module)
   (rtetb/block-formatting-tool :spoiler)
   (cut/cut-tool module)])

(defn full-rte-docconf [module placeholder]
  {:placeholder     placeholder
   :class           "full"
   :auto-correctors {:user (user/user-autocorrector)}
   :localizer       (:localizer module)
   :semantics       {:user {:logic     (user/user-material-semantics module)
                            :component (partial user/doc-user module)}
                     :cut  {:logic     (cut/cut-block-semantics module)
                            :component (partial cut/doc-cut module)}
                     :spoiler
                           {:logic     (spoiler/spoiler-block-semantics)
                            :component (partial spoiler/doc-spoiler module)}}})

(defn full-rte-opts [module placeholder]
  {:concat-tools (full-rte-concat-tools module)
   :doc/conf     (full-rte-docconf module placeholder)})


(defn light-rte-docconf [module placeholder]
  {:placeholder     placeholder
   :class           "light"
   :auto-correctors {:user (user/user-autocorrector)}
   :localizer       (:localizer module)
   :semantics       {:user {:logic     (user/user-material-semantics module)
                            :component (partial user/doc-user module)}}})

(defn light-rte-opts [module opts]
  (merge
    {:tools    [(rtetb/basic-formatting-tool :bold :key/b)
                (rtetb/basic-formatting-tool :italic :key/i)
                (rtetb/basic-formatting-tool :underscore :key/u)
                (rtetb/basic-formatting-tool :strikethrough :key/s)
                (rtetb/block-formatting-tool :blockquote)
                (rtetb/block-formatting-tool :list :unordered-list)
                (rtetb/block-formatting-tool :list :ordered-list)
                (rtetb/link-tool)
                (rtetb/image-tool)
                (rtetb/video-tool)
                (user/user-tool module)]
     :doc/conf (light-rte-docconf module (:placeholder opts))}
    (dissoc opts :placeholder)))

;;=============================
;; DOC RENDERING
;;=============================
(rum/defcs
  spoiler
  < (rum/local {:collapsed? true})
  [state module attrs childs]
  (let [l (:localizer module)
        local (:rum/local state)
        collapsed? (:collapsed? @local)]
    [:div.spoiler
     {:class (when collapsed? "collapsed")}
     [:span.title
      {:on-click #(swap! local assoc :collapsed? (not collapsed?))}
      (:title attrs
        (l :app.post/spoiler-default-title))]
     (vec (concat [:div.content] childs))]))

(defn prepare-generic-post-tag-attrs [attrs]
  (->
    attrs
    (dissoc :data-keks)))

(defn default-post-tag [tag attrs childs]
  (->>
    childs
    (concat [tag (prepare-generic-post-tag-attrs attrs)])
    (keep identity)
    vec))

(defn video-post-tag [_ attrs _]
  (let [data-keks (:data-keks attrs)
        data-keks (when data-keks (r/read-string data-keks))
        iframe-html (get-in data-keks [:data :embedding-code])
        {:keys [tag attrs]} (when iframe-html
                              (->
                                iframe-html
                                html/html->hz-body
                                z/down
                                z/node))]
    (when tag [tag attrs])))

(defn last-child-post-tag [_ _ childs]
  (last childs))

(defn ignore-post-tag [_ _ _])

(defn build-post-tag [module ev tag attrs childs]
  (cond
    (= tag :html) (last-child-post-tag tag attrs childs)
    (= tag :head) (ignore-post-tag tag attrs childs)
    (= tag :body) (default-post-tag :div (assoc attrs :class "body") childs)
    (= tag :video) (video-post-tag tag attrs childs)
    (and (= tag :a) (= (:id attrs) "cut"))
    (common/event-link module (assoc ev :qn :action.app/read-post) (dissoc attrs :data-keks) (first childs))
    (and (= tag :div) (= (:class attrs) "spoiler"))
    (spoiler module attrs childs)
    :else
    (default-post-tag tag attrs childs)))
