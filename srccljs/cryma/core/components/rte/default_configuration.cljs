(ns cryma.core.components.rte.default-configuration
  (:require [cryma.core.components.rte.model.semantics.text :as text]
            [cryma.core.components.rte.model.semantics.link :as link]
            [cryma.core.components.rte.model.semantics.media :as media]
            [cryma.core.components.rte.model.semantics.blocks :as blocks]
            [cryma.core.components.rte.model.history :as h]
            [cryma.core.components.rte.model.convertation :as convert]
            [cryma.core.components.rte.model.autocorrectors.link :as link-cor]
            [cryma.core.components.rte.materials :as materials]))

(def default-configuration
  {:localizer (fn [v] (str v))

   :placeholder
              nil

   :change-notification-timeout-ms
              200

   :auto-correctors
              {:link (link-cor/link-autocorrector)}

   :converters
              {:import convert/import-html-or-empty
               :export convert/export-html}

   :api
              {:doc.api/type-in
               (h/history-operation
                 :doc.history.api/append
                 200 h/history-join-type-in)
               :doc.api/del
               (h/history-operation
                 :doc.history.api/append
                 200 h/history-join-counter)
               :doc.api/backspace
               (h/history-operation
                 :doc.history.api/append
                 200 h/history-join-counter)
               :doc.api/new-line
               (h/history-operation
                 :doc.history.api/append
                 200 h/history-join-counter)
               :doc.api/cut
               (h/history-operation
                 :doc.history.api/append)
               :doc.api/paste
               (h/history-operation
                 :doc.history.api/append)
               :doc.api/basic-formatting
               (h/history-operation
                 :doc.history.api/append)
               :doc.api/block-formatting
               (h/history-operation
                 :doc.history.api/append)
               :doc.api/insert
               (h/history-operation
                 :doc.history.api/append)
               :doc.api/update
               (h/history-operation
                 :doc.history.api/append)
               :doc.api/undo
               (h/history-operation
                 :doc.history.api/undo)
               :doc.api/redo
               (h/history-operation
                 :doc.history.api/redo)}

   :formats
              {:bold          {}
               :italic        {}
               :underscore    {}
               :strikethrough {}}

   :semantics
              {:text       {:logic     (text/text-material-semantics)
                            :component materials/doc-text}
               :link       {:logic     (link/link-material-semantics)
                            :component materials/doc-link}
               :image      {:logic     (media/image-material-semantics)
                            :component materials/doc-img}
               :video      {:logic     (media/video-material-semantics)
                            :component materials/doc-video}
               :document   {:logic (blocks/document-block-semantics)}
               :list       {:logic (blocks/list-block-semantics)}
               :blockquote {:logic (blocks/generic-block-semantics :blockquote)}
               :p          {:logic (blocks/generic-block-semantics :p)}
               :h1         {:logic (blocks/generic-block-semantics :h1)}
               :h2         {:logic (blocks/generic-block-semantics :h2)}
               :h3         {:logic (blocks/generic-block-semantics :h3)}
               :h4         {:logic (blocks/generic-block-semantics :h4)}}})

