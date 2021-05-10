(ns cryma.core.components.rte.model.tests
  (:require
    [cryma.core.components.rte.model :as m]
    [cryma.core.components.rte.model.configuration :as c]
    [cryma.core.components.rte.model.selection :as s]
    [cljs.test :refer-macros [deftest is testing
                              run-tests use-fixtures]]
    [cryma.core.components.rte.model.history :as h]
    [cryma.core.components.rte.default-configuration :as default]
    [cryma.core.components.rte.model.configuration :as configuration]))

;;=============================
;; UTILS
;;=============================
(defn wrap-conf [doc]
  (configuration/with-conf
    doc
    (c/create-configuration default/default-configuration)))

(defn blocks [fill blocks]
  (loop [cnt 1 res ""]
    (if (<= cnt blocks)
      (recur (inc cnt)
             (str (apply str res (map (fn [_] fill) (range cnt))) " "))
      (subs res 0 (dec (count res))))))

(defn material-text [fill bcnt]
  {:type      :material
   :semantics :text
   :content   (blocks fill bcnt)})

(defn line-text [mat-specs & [line-semantics]]
  {:type      :line
   :semantics (or line-semantics :generic-line)
   :content   (mapv (fn [x]
                      (if (map? x)
                        x
                        (let [[fill bcnt] x]
                          (material-text fill bcnt))))
                    mat-specs)})

(defn block-text [line-specs block-semantics]
  {:type      :block
   :semantics block-semantics
   :content   (mapv (fn [x]
                      (if (map? x)
                        x
                        (let [[mat-spec line-semantics] x]
                          (line-text mat-spec line-semantics))))
                    line-specs)})

(defn doc-liner [line-specs]
  (wrap-conf
    (block-text line-specs :document)))

(defn doc-list [line-specs]
  (wrap-conf
    {:type
     :block
     :semantics
     :document
     :content
     [(block-text line-specs :list)]}))

(defn doc-full []
  (wrap-conf
    {:type
     :block
     :semantics
     :document
     :content
     [(line-text [["0" 3]])
      (block-text [[[["1" 2]]]
                   (block-text [[[["2" 1]]]] :list)
                   [[["3" 2]]]]
                  :list)
      (line-text [["4" 3]])
      ]}))

;;=============================
;; ACTIONs TESTs
;;=============================

(deftest normilize-path
  (let [doc (wrap-conf
              {:type
               :block
               :semantics
               :document
               :content
               [(block-text [[[["1" 2]]]
                             [[["2" 2]]]] :list)]})]
    (is (= (s/normalize-path doc [0 1] 0) [0 1 0 0]))))

;;-----------------------------
;; TYPE-IN
;;-----------------------------
(defn do-type-in [s & [path]]
  (m/do-action
    (m/open-document
      (doc-liner [[[["1" 6] ["2" 6]]]])
      path)
    {:qn  :doc.api/type-in
     :key s}))

#_(do-type-in "X")

(deftest type-in-formatted
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :text
                                :content   "222"}]}
                  ]}
          (wrap-conf)
          (m/open-document [0 0 1])
          (m/do-action {:qn     :doc.api/type-in :key "X"
                        :format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line
                           :content   [{:type      :material
                                        :semantics :text
                                        :content   "2"}
                                       {:type      :material
                                        :format    #{:bold}
                                        :semantics :text
                                        :content   "X"}
                                       {:type      :material
                                        :semantics :text
                                        :content   "22"}]}]}))
    (is (= selection (s/collapsed-selection [0 1 1])))))


(deftest type-in-image-formatted
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :text
                                :content   "222"}
                               {:type      :material
                                :semantics :image}]}
                  ]}
          (wrap-conf)
          (m/open-document [0 1 0])
          (m/do-action {:qn     :doc.api/type-in :key "X"
                        :format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line
                           :content   [{:type      :material
                                        :semantics :text
                                        :content   "222"}
                                       {:type      :material
                                        :semantics :text
                                        :format    #{:bold}
                                        :content   "X"}
                                       {:type      :material
                                        :semantics :image}]}]}))
    (is (= selection (s/collapsed-selection [0 1 1])))))

(deftest type-in-image
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :text
                                :format    #{:bold}
                                :content   "222"}
                               {:type      :material
                                :semantics :image}]}
                  ]}
          (wrap-conf)
          (m/open-document [0 1 0])
          (m/do-action {:qn     :doc.api/type-in :key "X"
                        :format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line
                           :content   [{:type      :material
                                        :semantics :text
                                        :format    #{:bold}
                                        :content   "222X"}
                                       {:type      :material
                                        :semantics :image}]}]}))
    (is (= selection (s/collapsed-selection [0 0 4])))))

(deftest type-in-image-line-start-formatted
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :image}]}]}
          (wrap-conf)
          (m/open-document [0 0 0])
          (m/do-action {:qn     :doc.api/type-in :key "X"
                        :format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line
                           :content   [{:type      :material
                                        :semantics :text
                                        :format    #{:bold}
                                        :content   "X"}
                                       {:type      :material
                                        :semantics :image}]}]}))
    (is (= selection (s/collapsed-selection [0 0 1])))))

(deftest type-in-image-line-start
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :image}]}]}
          (wrap-conf)
          (m/open-document [0 0 0])
          (m/do-action {:qn :doc.api/type-in :key "X"}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line
                           :content   [{:type      :material
                                        :semantics :text
                                        :content   "X"}
                                       {:type      :material
                                        :semantics :image}]}]}))
    (is (= selection (s/collapsed-selection [0 0 1])))))

(deftest type-in-image-doubled-formatted
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :image}
                               {:type      :material
                                :semantics :image}]}]}
          (wrap-conf)
          (m/open-document [0 1 0])
          (m/do-action {:qn     :doc.api/type-in :key "X"
                        :format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line
                           :content   [{:type      :material
                                        :semantics :image}
                                       {:type      :material
                                        :format    #{:bold}
                                        :semantics :text
                                        :content   "X"}
                                       {:type      :material
                                        :semantics :image}]}]}))
    (is (= selection (s/collapsed-selection [0 1 1])))))

(deftest type-in-image-doubled
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :image}
                               {:type      :material
                                :semantics :image}]}]}
          (wrap-conf)
          (m/open-document [0 1 0])
          (m/do-action {:qn :doc.api/type-in :key "X"}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line
                           :content   [{:type      :material
                                        :semantics :image}
                                       {:type      :material
                                        :semantics :text
                                        :content   "X"}
                                       {:type      :material
                                        :semantics :image}]}]}))
    (is (= selection (s/collapsed-selection [0 1 1])))))


(deftest type-in-image-doubled-back
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :image}
                               {:type      :material
                                :semantics :image}]}]}
          (wrap-conf)
          (m/open-document [0 0 1])
          (m/do-action {:qn     :doc.api/type-in :key "X"
                        :format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line
                           :content   [{:type      :material
                                        :semantics :image}
                                       {:type      :material
                                        :semantics :text
                                        :format    #{:bold}
                                        :content   "X"}
                                       {:type      :material
                                        :semantics :image}]}]}))
    (is (= selection (s/collapsed-selection [0 1 1])))))

(deftest type-in-image-line-end
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :image}]}]}
          (wrap-conf)
          (m/open-document [0 0 1])
          (m/do-action {:qn     :doc.api/type-in :key "X"
                        :format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line
                           :content   [{:type      :material
                                        :semantics :image}
                                       {:type      :material
                                        :semantics :text
                                        :format    #{:bold}
                                        :content   "X"}]}]}))
    (is (= selection (s/collapsed-selection [0 1 1])))))

(deftest type-in-line-start
  (let [{:keys [doc selection]} (do-type-in "X")]
    (is (= doc (doc-liner [[[{:type      :material,
                              :semantics :text,
                              :content   "X1 11 111 1111 11111 111111"}
                             ["2" 6]]]])))
    (is (= selection (s/collapsed-selection [0 0 1])))))

(deftest type-in-line-end
  (let [{:keys [doc selection]} (do-type-in "X" [0 1 26])]
    (is (= doc (doc-liner [[[["1" 6]
                             {:type      :material,
                              :semantics :text,
                              :content   "2 22 222 2222 22222 222222X"}]]])))
    (is (= selection (s/collapsed-selection [0 1 27])))))

;;-----------------------------
;; NEW-LINE
;;-----------------------------
(defn do-new-line [& [path doc]]
  (m/do-action
    (m/open-document
      (or doc (doc-liner [[[["1" 6] ["2" 6]]]]))
      path)
    {:qn :doc.api/new-line}))

(deftest new-line-line-start
  (let [{:keys [doc selection]} (do-new-line)]
    (is (= doc (doc-liner [[[["" 0]]]
                           [[["1" 6] ["2" 6]]]])))
    (is (= selection (s/collapsed-selection [1 0 0])))))

(deftest new-line-line-start-media
  (let [{:keys [doc selection]}
        (->
          (doc-liner [{:type      :line,
                       :semantics :generic-line,
                       :content   [{:type      :material,
                                    :semantics :video,
                                    :data      {:embedding-code ""}}]}])
          wrap-conf
          (m/open-document [0 0 0])
          (m/do-action {:qn :doc.api/new-line})
          )]
    (is (= doc (doc-liner [[[["" 0]]]
                           {:type      :line,
                            :semantics :generic-line,
                            :content   [{:type      :material,
                                         :semantics :video,
                                         :data      {:embedding-code ""}}]}])))
    (is (= selection (s/collapsed-selection [1 0 0])))))

(deftest new-line-line-middle-split
  (let [{:keys [doc selection]} (do-new-line [0 0 19])]
    (is (= doc (doc-liner [[[["1" 5]]]
                           [[{:type      :material,
                              :semantics :text,
                              :content   " 111111"}
                             ["2" 6]]]])))
    (is (= selection (s/collapsed-selection [1 0 0])))))

(deftest new-line-line-middle
  (let [{:keys [doc selection]} (do-new-line [0 0 26])]
    (is (= doc (doc-liner [[[["1" 6]]]
                           [[["" 0] ["2" 6]]]])))
    (is (= selection (s/collapsed-selection [1 0 0])))))

(deftest new-line-line-end
  (let [{:keys [doc selection]} (do-new-line [0 1 26])]
    (is (= doc (doc-liner [[[["1" 6] ["2" 6]]]
                           [[["" 0]]]])))
    (is (= selection (s/collapsed-selection [1 0 0])))))

(deftest new-line-line-end-media
  (let [{:keys [doc selection]}
        (->
          (doc-liner [{:type      :line,
                       :semantics :generic-line,
                       :content   [{:type      :material,
                                    :semantics :video,
                                    :data      {:embedding-code ""}}]}
                      [[["1" 1]]]])
          wrap-conf
          (m/open-document [0 0 1])
          (m/do-action {:qn :doc.api/new-line})
          )]
    (is (= doc (doc-liner [{:type      :line,
                            :semantics :generic-line,
                            :content   [{:type      :material,
                                         :semantics :video,
                                         :data      {:embedding-code ""}}]}
                           [[["" 0]]]
                           [[["1" 1]]]])))
    (is (= selection (s/collapsed-selection [1 0 0])))))

(deftest new-line-empty-line
  (let [{:keys [doc selection]} (do-new-line [1 0 0 0]
                                             (doc-liner [[[["1" 6]]]
                                                         [[["" 0]]]]))]
    (is (= doc (doc-liner [[[["1" 6]]]
                           [[["" 0]]]
                           [[["" 0]]]])))
    (is (= selection (s/collapsed-selection [2 0 0])))))

(deftest new-line-block-start
  (let [{:keys [doc selection]} (do-new-line [0 0 0 0]
                                             (doc-list [[[["1" 6]]]
                                                        [[["" 0]]]]))]
    (is (= doc (doc-list [[[["" 0]]]
                          [[["1" 6]]]
                          [[["" 0]]]])))
    (is (= selection (s/collapsed-selection [0 1 0 0])))))

(deftest new-line-block-middle
  (let [{:keys [doc selection]} (do-new-line [0 1 0 0]
                                             (doc-list [[[["1" 6]]]
                                                        [[["" 0]]]
                                                        [[["2" 6]]]]))]
    (is (= doc {:type
                :block
                :semantics
                :document
                :content
                [(block-text [[[["1" 6]]]] :list)
                 (line-text [["" 0]])
                 (block-text [[[["2" 6]]]] :list)
                 ]}))
    (is (= selection (s/collapsed-selection [1 0 0])))))

(deftest new-line-block-end
  (let [{:keys [doc selection]} (do-new-line [0 1 0 0]
                                             (doc-list [[[["1" 6]]]
                                                        [[["" 0]]]]))]
    (is (= doc {:type
                :block
                :semantics
                :document
                :content
                [(block-text [[[["1" 6]]]] :list)
                 (line-text [["" 0]])
                 ]}))
    (is (= selection (s/collapsed-selection [1 0 0])))))

(deftest new-line-block-end-doc-middle
  (let [{:keys [doc selection]} (do-new-line [0 2 0 0]
                                             (wrap-conf
                                               {:type
                                                :block
                                                :semantics
                                                :document
                                                :content
                                                [(block-text [[[["1" 6]]]
                                                              [[["2" 6]]]
                                                              [[["" 0]]]] :list)
                                                 (line-text [["" 0]])
                                                 ]}))]
    (is (= doc {:type
                :block
                :semantics
                :document
                :content
                [(block-text [[[["1" 6]]]
                              [[["2" 6]]]] :list)
                 (line-text [["" 0]])
                 (line-text [["" 0]])
                 ]}))
    (is (= selection (s/collapsed-selection [1 0 0])))))

;;-----------------------------
;; BACKSPACE
;;-----------------------------

(defn do-backspace [& [path]]
  (m/do-action
    (m/open-document
      (doc-full)
      path)
    {:qn :doc.api/backspace}))

#_(do-backspace [1 0 0 0 0])

(deftest backspace-material-border
  (let [{:keys [doc selection]}
        (->
          {:type
           :block
           :semantics
           :document
           :content
           [(line-text [{:type      :material
                         :semantics :text
                         :format    #{:bold}
                         :content   "1"}
                        ["2" 1]])]}
          (wrap-conf)
          (m/open-document [0 0 1])
          (m/do-action {:qn :doc.api/backspace}))]
    ;NOTE::snoop: there is an opinion that here we should have an empty bold material with selection pointing inside it But... this is something to think about
    (is (= doc {:type      :block,
                :semantics :document,
                :content   [(line-text [#_{:type      :material
                                           :semantics :text
                                           :format    #{:bold}
                                           :content   ""}
                                        ["2" 1]])]}))
    (is (= selection (s/collapsed-selection [0 0 0])))))

(deftest backspace-same-material-border
  (let [{:keys [doc selection]}
        (->
          {:type
           :block
           :semantics
           :document
           :content
           [(line-text [["1" 1]
                        ["2" 1]])]}
          (wrap-conf)
          (m/open-document [0 1 0])
          (m/do-action {:qn :doc.api/backspace}))]
    (is (= doc {:type      :block,
                :semantics :document,
                :content   [(line-text [["2" 1]])]}))
    (is (= selection (s/collapsed-selection [0 0 0])))))

(deftest backspace-same-material-border-through-empty
  (let [{:keys [doc selection]}
        (->
          {:type
           :block
           :semantics
           :document
           :content
           [(line-text [["1" 1]
                        ["" 0]
                        ["2" 1]])]}
          (wrap-conf)
          (m/open-document [0 2 0])
          (m/do-action {:qn :doc.api/backspace}))]
    (is (= doc {:type      :block,
                :semantics :document,
                :content   [(line-text [["2" 1]])]}))
    (is (= selection (s/collapsed-selection [0 0 0])))))

(deftest backspace-block-line-start
  (let [{:keys [doc selection]} (do-backspace [1 1 0 0 0])]
    (is (= doc {:type
                :block
                :semantics
                :document
                :content
                [(line-text [["0" 3]])
                 (block-text [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type      :material,
                                            :semantics :text,
                                            :content   "1 112"}]}
                              [[["3" 2]]]]
                             :list)
                 (line-text [["4" 3]])
                 ]}))
    (is (= selection (s/collapsed-selection [1 0 0 4])))))

(deftest backspace-block->generic-line
  (let [{:keys [doc selection]} (do-backspace [1 0 0 0 0])]
    (is (= doc {:type
                :block
                :semantics
                :document
                :content
                [(line-text [{:type      :material,
                              :semantics :text,
                              :content   "0 00 0001 11"}])
                 (block-text [(block-text [[[["2" 1]]]] :list)
                              [[["3" 2]]]]
                             :list)
                 (line-text [["4" 3]])
                 ]}))
    (is (= selection (s/collapsed-selection [0 0 8])))))

(deftest backspace-image-last-position
  (let [{:keys [doc selection]}
        (->
          {:type :block, :semantics :document, :content
                 [{:type :line, :semantics :generic-line, :content
                         [{:type :material, :semantics :text, :content "1"}
                          {:type :material, :semantics :image, :data {:src "222"}}]}]}
          (wrap-conf)
          (m/open-document [0 1 1])
          (m/do-action {:qn :doc.api/backspace}))]
    (is (= doc {:type :block, :semantics :document, :content
                      [{:type :line, :semantics :generic-line, :content
                              [{:type :material, :semantics :text, :content "1"}]}]}))
    (is (= selection (s/collapsed-selection [0 0 1])))))

(deftest backspace-just-image-last-position
  (let [{:keys [doc selection]}
        (->
          {:type :block, :semantics :document, :content
                 [{:type :line, :semantics :generic-line, :content
                         [{:type :material, :semantics :image, :data {:src "а"}}]}]}
          (wrap-conf)
          (m/open-document [0 0 1])
          (m/do-action {:qn :doc.api/backspace}))]
    (is (= doc {:type :block, :semantics :document, :content
                      [{:type :line, :semantics :generic-line, :content
                              [{:type :material, :semantics :text, :content ""}]}]}))
    (is (= selection (s/collapsed-selection [0 0 0])))))

(deftest backspace-just-image-noncollapsed
  (let [{:keys [doc selection]}
        (->
          {:type :block, :semantics :document, :content
                 [{:type :line, :semantics :generic-line, :content
                         [{:type :material, :semantics :image, :data {:src "a"}}]}]}
          (wrap-conf)
          (m/open-document [0 0 0])
          (s/update-selection {:start [0 0 0] :end [0 0 1]})
          (m/do-action {:qn :doc.api/backspace}))]
    (is (= doc {:type :block, :semantics :document, :content
                      [{:type :line, :semantics :generic-line, :content
                              [{:type :material, :semantics :text, :content ""}]}]}))
    (is (= selection (s/collapsed-selection [0 0 0])))))

;;-----------------------------
;; DEL
;;-----------------------------

(defn do-del [& [path]]
  (m/do-action
    (m/open-document
      (doc-full)
      path)
    {:qn :doc.api/del}))

#_(do-del [1 1 0 0 1])


(deftest del-material-border
  (let [{:keys [doc selection]}
        (->
          {:type
           :block
           :semantics
           :document
           :content
           [(line-text [{:type      :material
                         :semantics :text
                         :format    #{:bold}
                         :content   "1"}
                        ["2" 1]])]}
          (wrap-conf)
          (m/open-document [0 0 1])
          (m/do-action {:qn :doc.api/del}))]
    (is (= doc {:type      :block,
                :semantics :document,
                :content   [(line-text [{:type      :material
                                         :semantics :text
                                         :format    #{:bold}
                                         :content   "1"}])]}))
    (is (= selection (s/collapsed-selection [0 0 1])))))

(deftest del-block-line-start
  (let [{:keys [doc selection]} (do-del [1 1 0 0 0])]
    (is (= doc {:type
                :block
                :semantics
                :document
                :content
                [(line-text [["0" 3]])
                 (block-text [[[["1" 2]]]
                              (block-text [[[["" 1]]]] :list)
                              [[["3" 2]]]]
                             :list)
                 (line-text [["4" 3]])
                 ]}))
    (is (= selection (s/collapsed-selection [1 1 0 0 0])))))

(deftest del-block-line-end
  (let [{:keys [doc selection]} (do-del [1 1 0 0 1])]
    (is (= doc {:type
                :block
                :semantics
                :document
                :content
                [(line-text [["0" 3]])
                 (block-text [[[["1" 2]]]
                              (block-text
                                [[[{:type      :material,
                                    :semantics :text,
                                    :content   "23 33"}]]]
                                :list)]
                             :list)
                 (line-text [["4" 3]])
                 ]}))
    (is (= selection (s/collapsed-selection [1 1 0 0 1])))))

;;-----------------------------
;; COPY
;;-----------------------------
(defn do-copy [selection]
  (m/do-action
    {:doc       (doc-full)
     :selection selection
     :history   []}
    {:qn :doc.api/copy}))

#_(do-copy {:start [1 1 0 0 1] :end [2 0 8]})
#_(do-copy {:start [0 0 0] :end [2 0 8]})

(deftest copy-all
  (let [selection {:start [0 0 0] :end [2 0 8]}
        {:keys [clipboard] :as opendoc}
        (do-copy selection)]
    (is (= selection (:selection opendoc)))
    (is (= clipboard
           {:text/plain "0 00 000\n1 11\n2\n3 33\n4 44 444",
            :text/html  "<html><head></head><body data-keks=\"{:type :block, :semantics :document}\"><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">0 00 000</span></p><ul data-keks=\"{:type :block, :semantics :list}\"><li data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">1 11</span></li><ul data-keks=\"{:type :block, :semantics :list}\"><li data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">2</span></li></ul><li data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">3 33</span></li></ul><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">4 44 444</span></p></body></html>"}))))

(deftest copy-last-half
  (let [selection {:start [1 1 0 0 1] :end [2 0 8]}
        {:keys [clipboard] :as opendoc} (do-copy selection)]
    (is (= selection (:selection opendoc)))
    (is (= clipboard
           {:text/plain "\n3 33\n4 44 444",
            :text/html  "<html><head></head><body data-keks=\"{:type :block, :semantics :document}\"><ul data-keks=\"{:type :block, :semantics :list}\"><ul data-keks=\"{:type :block, :semantics :list}\"><li data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\"></span></li></ul><li data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">3 33</span></li></ul><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">4 44 444</span></p></body></html>"}))))

;;-----------------------------
;; CUT
;;-----------------------------

(defn do-cut [selection]
  (m/do-action
    {:doc       (doc-full)
     :selection selection
     :history   (h/init-history)}
    {:qn :doc.api/cut}))

#_(do-cut {:start [0 0 0] :end [2 0 8]})

(deftest cut-all
  (let [selection {:start [0 0 0] :end [2 0 8]}
        {:keys [selection doc clipboard]} (do-cut selection)]
    (is (= selection (s/collapsed-selection [0 0 0])))
    (is (= doc (doc-liner [[[["" 0]]]])))
    (is (= clipboard
           {:text/plain "0 00 000\n1 11\n2\n3 33\n4 44 444",
            :text/html  "<html><head></head><body data-keks=\"{:type :block, :semantics :document}\"><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">0 00 000</span></p><ul data-keks=\"{:type :block, :semantics :list}\"><li data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">1 11</span></li><ul data-keks=\"{:type :block, :semantics :list}\"><li data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">2</span></li></ul><li data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">3 33</span></li></ul><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">4 44 444</span></p></body></html>"}))))

(deftest cut-last-half
  (let [selection {:start [1 1 0 0 1] :end [2 0 8]}
        {:keys [doc selection clipboard]} (do-cut selection)]
    (is (= selection (s/collapsed-selection [1 1 0 0 1])))
    (is (= doc
           {:type
            :block
            :semantics
            :document
            :content
            [(line-text [["0" 3]])
             (block-text [[[["1" 2]]]
                          (block-text [[[["2" 1]]]] :list)]
                         :list)
             ]}))
    (is (= clipboard
           {:text/plain "\n3 33\n4 44 444",
            :text/html  "<html><head></head><body data-keks=\"{:type :block, :semantics :document}\"><ul data-keks=\"{:type :block, :semantics :list}\"><ul data-keks=\"{:type :block, :semantics :list}\"><li data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\"></span></li></ul><li data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">3 33</span></li></ul><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">4 44 444</span></p></body></html>"}))))

;;-----------------------------
;; PASTE
;;-----------------------------

(defn do-paste [selection & [doc]]
  (let [{:keys [_ clipboard]} (do-cut selection)]
    (m/do-action
      {:doc       (or doc (doc-full))
       :selection (s/collapsed-selection (:start selection))
       :history   (h/init-history)}
      {:qn        :doc.api/paste
       :clipboard clipboard})))

#_(do-paste {:start [0 0 0] :end [2 0 8]}
            (wrap-conf
              {:type :block,
               :semantics :document,
               :content [{:type :line,
                          :semantics :generic-line,
                          :content [{:type :material, :semantics :text, :content "0 00 000"}]}
                         {:type :line,
                          :semantics :generic-line,
                          :content [{:type :material, :semantics :text, :content ""}]}
                         {:type :line,
                          :semantics :generic-line,
                          :content [{:type :material, :semantics :text, :content "4 44 444"}]}]}))

#_(do-paste {:start [0 0 1] :end [2 0 8]})

(deftest paste-media
  (let [odoc {:doc       (wrap-conf
                           (doc-liner [{:type      :line,
                                        :semantics :generic-line,
                                        :content   [{:type      :material,
                                                     :semantics :image,
                                                     :data      {:src ""}}]}
                                       [[["1" 2]]]
                                       ]))
              :selection {:start [0 0 0] :end [0 0 1]}
              :history   (h/init-history)}
        clipboard (:clipboard (m/do-action odoc {:qn :doc.api/copy}))
        {:keys [doc selection]}
        (->
          (assoc odoc :selection {:start [1 0 1] :end [1 0 1]})
          (m/do-action {:qn        :doc.api/paste
                        :clipboard clipboard}))]
    (is (= doc
           (doc-liner [{:type      :line,
                        :semantics :generic-line,
                        :content   [{:type      :material,
                                     :semantics :image,
                                     :data      {:src ""}}]}
                       {:type      :line,
                        :semantics :generic-line,
                        :content   [{:type      :material,
                                     :semantics :text, :content "1"}
                                    {:type      :material,
                                     :semantics :image, :content "",
                                     :data      {:src ""}}
                                    {:type    :material, :semantics :text,
                                     :content " 11"}]}])))
    (is (= selection (s/collapsed-selection [1 1 1])))))

(deftest paste-media-empty-line
  (let [odoc {:doc       (wrap-conf
                           (doc-liner [{:type      :line,
                                        :semantics :generic-line,
                                        :content   [{:type      :material,
                                                     :semantics :image,
                                                     :data      {:src ""}}]}
                                       [[["" 0]]]
                                       ]))
              :selection {:start [0 0 0] :end [0 0 1]}
              :history   (h/init-history)}
        clipboard (:clipboard (m/do-action odoc {:qn :doc.api/copy}))
        {:keys [doc selection]}
        (->
          (assoc odoc :selection {:start [1 0 0] :end [1 0 0]})
          (m/do-action {:qn        :doc.api/paste
                        :clipboard clipboard}))]
    (is (= doc
           (doc-liner [{:type      :line,
                        :semantics :generic-line,
                        :content   [{:type      :material,
                                     :semantics :image,
                                     :data      {:src ""}}]}
                       {:type      :line,
                        :semantics :generic-line,
                        :content   [{:type      :material,
                                     :semantics :image, :content "",
                                     :data      {:src ""}}]}])))
    (is (= selection (s/collapsed-selection [1 0 1])))))


(deftest past-all
  (let [{:keys [doc selection]} (do-paste {:start [0 0 0] :end [2 0 8]})]
    (is (= doc
           {:type
            :block
            :semantics
            :document
            :content
            [(line-text [["0" 3]])
             (block-text [[[["1" 2]]]
                          (block-text [[[["2" 1]]]] :list)
                          [[["3" 2]]]]
                         :list)
             (line-text [{:semantics :text
                          :content   "4 44 4440 00 000"
                          :type      :material}])
             (block-text [[[["1" 2]]]
                          (block-text [[[["2" 1]]]] :list)
                          [[["3" 2]]]]
                         :list)
             (line-text [["4" 3]])
             ]}))
    (is (= selection (s/collapsed-selection [2 0 8])))))

;; this one checks block nesting
#_(deftest past-last-half
    (let [{:keys [doc selection]} (do-paste {:start [1 1 0 0 1] :end [2 0 8]})]
      (is (= doc
             {:type
              :block
              :semantics
              :document
              :content
              [(line-text [["0" 3]])
               (block-text [[[["1" 2]]]
                            (block-text [[[["2" 1]]]
                                         (block-text [[[["3" 2]]]] :list)
                                         [[["4" 3]]]]
                                        :list)
                            [[["3" 2]]]]
                           :list)
               (line-text [["4" 3]])
               ]}))
      (is (= selection (s/collapsed-selection [1 1 2 0 8])))))

;; this one doesnt
(deftest past-last-half
  (let [{:keys [doc selection]} (do-paste {:start [1 1 0 0 1] :end [2 0 8]})]
    (is (= doc
           {:type
            :block
            :semantics
            :document
            :content
            [(line-text [["0" 3]])
             (block-text [[[["1" 2]]]
                          (block-text [[[["2" 1]]]
                                       [[["3" 2]]]
                                       [[["4" 3]]]]
                                      :list)
                          [[["3" 2]]]]
                         :list)
             (line-text [["4" 3]])
             ]}))
    (is (= selection (s/collapsed-selection [1 1 2 0 8])))))

;;-----------------------------
;; BASIC FORMATTING
;;-----------------------------
#_(->
    {:type :block
     :content
           [{:type      :line
             :semantics :generic-line
             :content   [{:type      :material
                          :semantics :text
                          :content   "222"}]}
            ]}
    (wrap-conf)
    (m/open-document)
    (m/update-selection {:start [0 0 1] :end [0 0 2]})
    (m/do-action {:qn         :doc.api/basic-formatting
                  :add-format #{:bold}}))

(deftest basic-formatting-simple-start
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :text
                                :content   "222"}]}
                  ]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0] :end [0 0 1]})
          (m/do-action {:qn         :doc.api/basic-formatting
                        :add-format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line,
                           :content   [{:type    :material, :semantics :text,
                                        :content "2", :format #{:bold}}
                                       {:type    :material, :semantics :text,
                                        :content "22"}]}]}))
    (is (= selection {:start [0 0 0] :end [0 0 1]}))))

(deftest basic-formatting-simple-middle
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :text
                                :content   "222"}]}
                  ]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 1] :end [0 0 2]})
          (m/do-action {:qn         :doc.api/basic-formatting
                        :add-format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line,
                           :content   [{:type    :material, :semantics :text,
                                        :content "2"}
                                       {:type    :material, :semantics :text,
                                        :content "2", :format #{:bold}}
                                       {:type    :material, :semantics :text,
                                        :content "2"}]}]}))
    (is (= selection {:start [0 0 1] :end [0 1 1]}))))

(deftest basic-formatting-simple-end
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :text
                                :content   "222"}]}
                  ]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 1] :end [0 0 3]})
          (m/do-action {:qn         :doc.api/basic-formatting
                        :add-format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line,
                           :content   [{:type    :material, :semantics :text,
                                        :content "2"}
                                       {:type    :material, :semantics :text,
                                        :content "22", :format #{:bold}}]}]}))
    (is (= selection {:start [0 0 1] :end [0 1 2]}))))

(deftest basic-formatting-middle-gluing
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type    :material, :semantics :text,
                                :content "2", :format #{:bold}}
                               {:type    :material, :semantics :text,
                                :content "22"}]}
                  ]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 1] :end [0 1 1]})
          (m/do-action {:qn         :doc.api/basic-formatting
                        :add-format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line,
                           :content   [{:type    :material, :semantics :text,
                                        :content "22", :format #{:bold}}
                                       {:type    :material, :semantics :text,
                                        :content "2"}]}]}))
    (is (= selection {:start [0 0 1] :end [0 0 2]}))))

(deftest basic-formatting-middle-gluing-mixed
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type    :material, :semantics :text,
                                :content "11", :format #{:bold}}
                               {:type    :material, :semantics :text,
                                :content "22"}
                               {:type      :material,
                                :semantics :image,
                                :data      {:src ""}}
                               {:type      :material,
                                :semantics :image,
                                :data      {:src ""}}
                               {:type    :material, :semantics :text,
                                :content "33"}
                               {:type    :material, :semantics :text,
                                :content "44", :format #{:bold}}
                               ]}
                  ]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 1] :end [0 4 2]})
          (m/do-action {:qn         :doc.api/basic-formatting
                        :add-format #{:bold}}))]
    (is (= doc {:type
                :block,
                :content
                [{:type      :line,
                  :semantics :generic-line,
                  :content   [{:type    :material, :semantics :text,
                               :content "1122", :format #{:bold}}
                              {:type :material, :semantics :image,
                               :data {:src ""}, :format #{:bold}}
                              {:type :material, :semantics :image,
                               :data {:src ""}, :format #{:bold}}
                              {:type    :material, :semantics :text,
                               :content "3344", :format #{:bold}}]}]}))
    (is (= selection {:start [0 0 1] :end [0 3 2]}))))

(deftest basic-formatting-middle-gluing-media
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type    :material, :semantics :text,
                                :content "11", :format #{:bold}}
                               {:type    :material, :semantics :text,
                                :content "22"}
                               {:type      :material,
                                :semantics :image,
                                :data      {:src ""}}
                               {:type      :material,
                                :semantics :image,
                                :data      {:src ""}}
                               {:type    :material, :semantics :text,
                                :content "33"}
                               {:type    :material, :semantics :text,
                                :content "44", :format #{:bold}}
                               ]}
                  ]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 2 0] :end [0 3 1]})
          (m/do-action {:qn         :doc.api/basic-formatting
                        :add-format #{:bold}}))]
    (is (= doc {:type
                :block,
                :content
                [{:type      :line,
                  :semantics :generic-line,
                  :content   [{:type    :material, :semantics :text,
                               :content "11", :format #{:bold}}
                              {:type    :material, :semantics :text,
                               :content "22"}
                              {:type      :material,
                               :semantics :image,
                               :data      {:src ""}
                               :format    #{:bold}}
                              {:type      :material,
                               :semantics :image,
                               :data      {:src ""}
                               :format    #{:bold}}
                              {:type    :material, :semantics :text,
                               :content "33"}
                              {:type    :material, :semantics :text,
                               :content "44", :format #{:bold}}
                              ]}]}))
    (is (= selection {:start [0 1 2] :end [0 3 1]}))))

(deftest basic-formatting-just-media
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material,
                                :semantics :image,
                                :data      {:src ""}}
                               {:type      :material,
                                :semantics :image,
                                :data      {:src ""}}]}
                  ]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0] :end [0 1 1]})
          (m/do-action {:qn         :doc.api/basic-formatting
                        :add-format #{:bold}}))]
    (is (= doc {:type
                :block,
                :content
                [{:type      :line,
                  :semantics :generic-line,
                  :content   [{:type      :material,
                               :semantics :image,
                               :data      {:src ""}
                               :format    #{:bold}}
                              {:type      :material,
                               :semantics :image,
                               :data      {:src ""}
                               :format    #{:bold}}]}]}))
    (is (= selection {:start [0 0 0] :end [0 1 1]}))))


(deftest basic-formatting-middle-gluing-remove
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type    :material, :semantics :text,
                                :content "2", :format #{:bold}}
                               {:type    :material, :semantics :text,
                                :content "22"}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0] :end [0 0 1]})
          (m/do-action {:qn            :doc.api/basic-formatting
                        :remove-format #{:bold}}))]
    (is (= doc {:type    :block,
                :content [{:type      :line,
                           :semantics :generic-line,
                           :content   [{:type    :material, :semantics :text,
                                        :content "222", :format #{}}]}]}))
    (is (= selection {:start [0 0 0] :end [0 0 1]}))))

(deftest basic-formatting-all
  (let [{:keys [doc selection]}
        (->
          {:type
           :block
           :semantics
           :document
           :content
           [(block-text [[[["1" 1]
                           {:type    :material, :semantics :text,
                            :content "22", :format #{:bold}}]]] :list)
            (line-text [["" 0]])
            (block-text [[[["2" 2]]]] :list)
            ]}
          wrap-conf
          (m/open-document)
          (s/update-selection {:start [0 0 0 0] :end [2 0 0 1]})
          (m/do-action {:qn         :doc.api/basic-formatting
                        :add-format #{:bold}}))]
    (is (= doc {:type
                :block,
                :semantics
                :document,
                :content
                [{:type      :block,
                  :semantics :list,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type      :material,
                                            :semantics :text,
                                            :content   "122",
                                            :format    #{:bold}}]}]}
                 {:type      :line,
                  :semantics :generic-line,
                  :content   [{:type      :material,
                               :semantics :text,
                               :content   "",
                               :format    #{:bold}}]}
                 {:type      :block,
                  :semantics :list,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type      :material,
                                            :semantics :text,
                                            :content   "2",
                                            :format    #{:bold}}
                                           {:type      :material,
                                            :semantics :text,
                                            :content   " 22"}]}]}]}))
    (is (= selection {:start [0 0 0 0] :end [2 0 0 1]}))))


(deftest basic-formatting-half
  (let [{:keys [doc selection]}
        (->
          {:type
           :block
           :semantics
           :document
           :content
           [(block-text [[[["1" 1]
                           {:type    :material, :semantics :text,
                            :content "22", :format #{:bold}}]]] :list)
            (line-text [["" 0]])
            (block-text [[[["2" 2]]]] :list)
            ]}
          wrap-conf
          (m/open-document)
          (s/update-selection {:start [1 0 0] :end [2 0 0 1]})
          (m/do-action {:qn         :doc.api/basic-formatting
                        :add-format #{:bold}}))]
    (is (= doc {:type
                :block,
                :semantics
                :document,
                :content
                [(block-text [[[["1" 1]
                                {:type    :material, :semantics :text,
                                 :content "22", :format #{:bold}}]]] :list)
                 {:type      :line,
                  :semantics :generic-line,
                  :content   [{:type      :material,
                               :semantics :text,
                               :content   "",
                               :format    #{:bold}}]}
                 {:type      :block,
                  :semantics :list,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type      :material,
                                            :semantics :text,
                                            :content   "2",
                                            :format    #{:bold}}
                                           {:type      :material,
                                            :semantics :text,
                                            :content   " 22"}]}]}]}))
    (is (= selection {:start [1 0 0] :end [2 0 0 1]}))))

(deftest basic-formatting-mixed-selection
  (let [{:keys [doc selection]}
        (->
          {:type
           :block
           :semantics
           :document
           :content
           [(block-text [[[["1" 1]
                           {:type    :material, :semantics :text,
                            :content "22", :format #{:bold}}]]] :list)
            (line-text [["" 0]])
            (block-text [[[["2" 2]]]
                         [[{:type    :material, :semantics :text,
                            :content "33", :format #{:bold}}]]] :list)
            ]}
          wrap-conf
          (m/open-document)
          (s/update-selection {:start [0 0 1 1] :end [2 1 0 1]})
          (m/do-action {:qn         :doc.api/basic-formatting
                        :add-format #{:bold}}))]
    (is (= doc {:type
                :block,
                :semantics
                :document,
                :content
                [{:type      :block,
                  :semantics :list,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content "1"}
                                           {:type    :material, :semantics :text,
                                            :content "22", :format #{:bold}}]}]}
                 {:type      :line,
                  :semantics :generic-line,
                  :content   [{:type    :material, :semantics :text,
                               :content "", :format #{:bold}}]}
                 {:type      :block,
                  :semantics :list,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content "2 22", :format #{:bold}}]}
                              {:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content "33", :format #{:bold}}]}]}]}))
    (is (= selection {:start [0 0 1 1] :end [2 1 0 1]}))))

;;-----------------------------
;; BLOCK FORMATTING
;;-----------------------------

(deftest block-formatting-line-collapsed
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :line
                   :semantics :generic-line
                   :content   [{:type      :material
                                :semantics :text
                                :content   "222"}]}
                  ]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 1] :end [0 0 1]})
          (m/do-action {:qn         :doc.api/block-formatting
                        :add-format {:semantics :list
                                     :format    #{:ordered-list}}}))]
    (is (= doc {:type
                :block,
                :content
                [{:type      :block
                  :format    #{:ordered-list}
                  :semantics :list
                  :content   [{:type      :line
                               :semantics :generic-line
                               :content   [{:type      :material
                                            :semantics :text
                                            :content   "222"}]}]}]}))
    (is (= selection {:start [0 0 0 1] :end [0 0 0 1]}))))

(deftest block-formatting-line-list
  (let [{:keys [doc selection]}
        (->
          {:type :block
           :content
                 [{:type      :block,
                   :semantics :list,
                   :content   [{:type      :line,
                                :semantics :generic-line,
                                :content   [{:type    :material, :semantics :text,
                                             :content "1"}
                                            {:type    :material, :semantics :text,
                                             :content "22", :format #{:bold}}]}]}
                  {:type      :line,
                   :semantics :generic-line,
                   :content   [{:type    :material, :semantics :text,
                                :content "", :format #{:bold}}]}
                  {:type      :block,
                   :semantics :list,
                   :content   [{:type      :line,
                                :semantics :generic-line,
                                :content   [{:type    :material, :semantics :text,
                                             :content "2 22", :format #{:bold}}]}
                               {:type      :line,
                                :semantics :generic-line,
                                :content   [{:type    :material, :semantics :text,
                                             :content "33", :format #{:bold}}]}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [1 0 0] :end [2 1 0 1]})
          (m/do-action {:qn         :doc.api/block-formatting
                        :add-format {:semantics :list
                                     :format    #{:ordered-list}}}))]
    (is (= doc {:type
                :block,
                :content
                [{:type      :block,
                  :semantics :list,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content "1"}
                                           {:type    :material, :semantics :text,
                                            :content "22", :format #{:bold}}]}]}
                 {:semantics :list,
                  :format    #{:ordered-list},
                  :type      :block,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content "", :format #{:bold}}]}
                              {:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content "2 22", :format #{:bold}}]}
                              {:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content "33", :format #{:bold}}]}]}]}))
    (is (= selection {:start [1 0 0 0] :end [1 2 0 1]}))))

(deftest block-formatting-whole-list
  (let [{:keys [doc selection]}
        (->
          {:type    :block,
           :content [{:semantics :list,
                      :format    #{:unordered-list},
                      :type      :block,
                      :content   [{:type      :line,
                                   :semantics :generic-line,
                                   :content   [{:type    :material, :semantics :text,
                                                :content "1111"}]}
                                  {:type      :line,
                                   :semantics :generic-line,
                                   :content   [{:type    :material, :semantics :text,
                                                :content "2222"}]}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0 0] :end [0 1 0 4]})
          (m/do-action {:qn         :doc.api/block-formatting
                        :add-format {:semantics :list
                                     :format    #{:ordered-list}}}))]
    (is (= doc {:type
                :block,
                :content
                [{:semantics :list,
                  :format    #{:ordered-list},
                  :type      :block,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content "1111"}]}
                              {:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content "2222"}]}]}]}))
    (is (= selection {:start [0 0 0 0] :end [0 1 0 4]}))))


;;-----------------------------
;; INSERTION
;;-----------------------------

(deftest insertion-material
  (let [{:keys [doc selection]}
        (->
          {:type
           :block,
           :content
           [{:semantics :list,
             :format    #{:unordered-list},
             :type      :block,
             :content   [{:type      :line,
                          :semantics :generic-line,
                          :content   [{:type      :material,
                                       :semantics :text,
                                       :format    #{:bold},
                                       :content   "1111"}]}
                         {:type      :line,
                          :semantics :generic-line,
                          :content   [{:type    :material, :semantics :text,
                                       :content "2222"}]}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0 1] :end [0 0 0 1]})
          (m/do-action {:qn   :doc.api/insert
                        :item {:type      :material
                               :semantics :image
                               :data      {:src ""}}}))]
    (is (= doc {:type
                :block,
                :content
                [{:semantics :list,
                  :format    #{:unordered-list},
                  :type      :block,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type      :material,
                                            :semantics :text,
                                            :format    #{:bold},
                                            :content   "1"}
                                           {:type      :material,
                                            :semantics :image,
                                            :content   ""
                                            :data      {:src ""}}
                                           {:type      :material,
                                            :semantics :text,
                                            :format    #{:bold},
                                            :content   "111"}]}
                              {:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content "2222"}]}]}]}))
    (is (= selection {:start [0 0 1 1] :end [0 0 1 1]}))))

(deftest insertion-material-non-collapsed
  (let [{:keys [doc selection]}
        (->
          {:type
           :block,
           :content
           [{:semantics :list,
             :format    #{:unordered-list},
             :type      :block,
             :content   [{:type      :line,
                          :semantics :generic-line,
                          :content   [{:type      :material,
                                       :semantics :text,
                                       :format    #{:bold},
                                       :content   "1111"}]}
                         {:type      :line,
                          :semantics :generic-line,
                          :content   [{:type    :material, :semantics :text,
                                       :content "2222"}]}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0 1] :end [0 1 0 3]})
          (m/do-action {:qn   :doc.api/insert
                        :item {:type      :material
                               :semantics :image
                               :data      {:src ""}}}))]
    (is (= doc {:type
                :block,
                :content
                [{:semantics :list,
                  :format    #{:unordered-list},
                  :type      :block,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type      :material,
                                            :semantics :text,
                                            :format    #{:bold},
                                            :content   "1"}
                                           {:type      :material,
                                            :semantics :image,
                                            :content   ""
                                            :data      {:src ""}}
                                           {:type      :material,
                                            :semantics :text,
                                            :content   "2"}]}]}]}))
    (is (= selection {:start [0 0 1 1] :end [0 0 1 1]}))))

(deftest insertion-wrap-link
  (let [{:keys [doc selection]}
        (->
          {:type
           :block,
           :content
           [{:semantics :list,
             :format    #{:unordered-list},
             :type      :block,
             :content   [{:type      :line,
                          :semantics :generic-line,
                          :content   [{:type      :material,
                                       :semantics :text,
                                       :format    #{:bold},
                                       :content   "1111"}]}
                         {:type      :line,
                          :semantics :generic-line,
                          :content   [{:type    :material, :semantics :text,
                                       :content "2222"}]}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0 1] :end [0 1 0 3]})
          (m/do-action {:qn              :doc.api/insert
                        :item            {:type      :material
                                          :semantics :link
                                          :data      {:href ""}}
                        :wrap-selection? :wrap-selection!}))]
    (is (= doc {:type
                :block,
                :content
                [{:semantics :list,
                  :format    #{:unordered-list},
                  :type      :block,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type   :material, :semantics :text,
                                            :format #{:bold}, :content "1"}
                                           {:type   :material, :semantics :link,
                                            :format #{:bold}
                                            :meta   {:original-semantics :text},
                                            :data   {:href ""}, :content "111 222"}
                                           {:type    :material, :semantics :text,
                                            :content "2"}]}]}]}))
    (is (= selection {:start [0 0 1 7], :end [0 0 1 7]}))))

(deftest insertion-wrap-link-image
  (let [{:keys [doc selection]}
        (->
          {:type
           :block,
           :content
           [{:type      :line,
             :semantics :generic-line,
             :content   [{:type      :material,
                          :semantics :text,
                          :content   "1111"}
                         {:type :material, :semantics :image,
                          :data {:src "src"}}
                         ]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 1 0] :end [0 1 1]})
          (m/do-action {:qn              :doc.api/insert
                        :item            {:type      :material
                                          :semantics :link
                                          :data      {:href "href"}}
                        :wrap-selection? :wrap-selection!}))]
    (is (= doc {:type
                :block,
                :content
                [{:type      :line,
                  :semantics :generic-line,
                  :content   [{:type    :material, :semantics :text,
                               :content "1111"}
                              {:type :material, :semantics :link,
                               :meta {:original-semantics :image},
                               :data {:src "src" :href "href"}}]}]}))
    (is (= selection {:start [0 1 1], :end [0 1 1]}))))

(deftest insertion-wrap-link-image-collapsed
  (let [{:keys [doc selection]}
        (->
          {:type
           :block,
           :content
           [{:type      :line,
             :semantics :generic-line,
             :content   [{:type :material, :semantics :image,
                          :data {:src "src"}}
                         ]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 1] :end [0 0 1]})
          (m/do-action {:qn              :doc.api/insert
                        :item            {:type      :material
                                          :semantics :link
                                          :data      {:href "href"}}
                        :wrap-selection? :wrap-selection!}))]
    (is (= doc {:type
                :block,
                :content
                [{:type      :line,
                  :semantics :generic-line,
                  :content   [{:type :material, :semantics :image, :data {:src "src"}}
                              {:type      :material,
                               :semantics :link,
                               :content   "",
                               :meta      {:original-semantics :text},
                               :data      {:href "href"}}]}]}))
    (is (= selection {:start [0 1 0], :end [0 1 0]}))))

(deftest insertion-wrap-link-link
  (let [{:keys [doc selection]}
        (->
          {:type
           :block,
           :content
           [{:type      :line,
             :semantics :generic-line,
             :content   [{:type    :material, :semantics :text,
                          :content "1111"}
                         {:type :material, :semantics :link,
                          :meta {:original-semantics :image},
                          :data {:src "src" :href "href"}}
                         ]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0] :end [0 1 1]})
          (m/do-action {:qn              :doc.api/insert
                        :item            {:type      :material
                                          :semantics :link
                                          :data      {:href "new-href"}}
                        :wrap-selection? :wrap-selection!}))]
    (is (= doc {:type
                :block,
                :content
                [{:type      :line,
                  :semantics :generic-line,
                  :content   [{:type    :material, :semantics :link,
                               :meta    {:original-semantics :text},
                               :data    {:href "new-href"}
                               :content "1111"}
                              {:type :material, :semantics :link,
                               :meta {:original-semantics :image},
                               :data {:src "src" :href "new-href"}}]}]}))
    (is (= selection {:start [0 1 1], :end [0 1 1]}))))

(deftest insertion-block
  (let [{:keys [doc selection]}
        (->
          {:type
           :block,
           :content
           [{:semantics :list,
             :format    #{:unordered-list},
             :type      :block,
             :content   [{:type      :line,
                          :semantics :generic-line,
                          :content   [{:type      :material,
                                       :semantics :text,
                                       :format    #{:bold},
                                       :content   "1111"}]}
                         {:type      :line,
                          :semantics :generic-line,
                          :content   [{:type    :material, :semantics :text,
                                       :content "2222"}]}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0 1] :end [0 0 0 1]})
          (m/do-action {:qn   :doc.api/insert
                        :item {:type      :block
                               :semantics :list
                               :format    #{:ordered-list}
                               :content   [{:type      :line,
                                            :semantics :generic-line,
                                            :content   [{:type      :material,
                                                         :semantics :text,
                                                         :content   ""}]}]}}))]
    (is (= doc {:type
                :block,
                :content
                [{:semantics :list,
                  :format    #{:unordered-list},
                  :type      :block,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type   :material, :semantics :text,
                                            :format #{:bold}, :content "1"}]}]}
                 {:type      :block,
                  :semantics :list,
                  :format    #{:ordered-list},
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content ""}]}]}
                 {:semantics :list,
                  :format    #{:unordered-list},
                  :type      :block,
                  :content   [{:type      :line,
                               :semantics :generic-line,
                               :content   [{:type   :material, :semantics :text,
                                            :format #{:bold}, :content "111"}]}
                              {:type      :line,
                               :semantics :generic-line,
                               :content   [{:type    :material, :semantics :text,
                                            :content "2222"}]}]}]}))
    (is (= selection {:start [1 0 0 0], :end [1 0 0 0]}))))

(deftest insertion-block-wrap
  (let [{:keys [doc selection]}
        (->
          {:type
           :block,
           :content
           [{:semantics :list,
             :format    #{:unordered-list},
             :type      :block,
             :content   [{:type      :line,
                          :semantics :generic-line,
                          :content   [{:type      :material,
                                       :semantics :text,
                                       :format    #{:bold},
                                       :content   "1111"}]}
                         {:type      :line,
                          :semantics :generic-line,
                          :content   [{:type    :material, :semantics :text,
                                       :content "2222"}]}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0 1] :end [0 1 0 3]})
          (m/do-action {:qn              :doc.api/insert
                        :wrap-selection? :wrap-selection!
                        :item            {:type      :block
                                          :semantics :list
                                          :format    #{:ordered-list}
                                          :content   [{:type      :line,
                                                       :semantics :generic-line,
                                                       :content   [{:type      :material,
                                                                    :semantics :text,
                                                                    :content   ""}]}]}}))]
    (is (= doc
           {:type
            :block,
            :content
            [{:semantics :list,
              :format    #{:unordered-list},
              :type      :block,
              :content   [{:type      :line,
                           :semantics :generic-line,
                           :content   [{:type   :material, :semantics :text,
                                        :format #{:bold}, :content "1"}]}]}
             {:type      :block,
              :semantics :list,
              :format    #{:ordered-list},
              :content   [{:type      :line,
                           :semantics :generic-line,
                           :content   [{:type   :material, :semantics :text,
                                        :format #{:bold}, :content "111"}]}
                          {:type      :line,
                           :semantics :generic-line,
                           :content   [{:type    :material, :semantics :text,
                                        :content "222"}]}]}
             {:semantics :list,
              :format    #{:unordered-list},
              :type      :block,
              :content   [{:type      :line,
                           :semantics :generic-line,
                           :content   [{:type    :material, :semantics :text,
                                        :content "2"}]}]}]}))
    (is (= selection {:start [1 1 0 3], :end [1 1 0 3]}))))

;;-----------------------------
;; UPDATING
;;-----------------------------

(deftest updating-media-collapsed
  (let [{:keys [doc selection]}
        (->
          {:type
           :block,
           :content
           [{:type      :line,
             :semantics :generic-line,
             :content   [{:type      :material,
                          :semantics :image,
                          :data      {:src ""}}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 1] :end [0 0 1]})
          (m/do-action {:qn   :doc.api/update
                        :item {:type      :material
                               :semantics :image,
                               :data      {:src "2"}}}))]
    (is (= doc {:type
                :block,
                :content
                [{:type      :line,
                  :semantics :generic-line,
                  :content   [{:type      :material,
                               :semantics :image,
                               :data      {:src "2"}}]}]}))
    (is (= selection {:start [0 0 1] :end [0 0 1]}))))

(deftest updating-media
  (let [{:keys [doc selection]}
        (->
          {:type
           :block,
           :content
           [{:type      :line,
             :semantics :generic-line,
             :content   [{:type      :material,
                          :semantics :image,
                          :data      {:src ""}}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0] :end [0 0 1]})
          (m/do-action {:qn   :doc.api/update
                        :item {:type      :material,
                               :semantics :image,
                               :data      {:src "2"}}}))]
    (is (= doc {:type
                :block,
                :content
                [{:type      :line,
                  :semantics :generic-line,
                  :content   [{:type      :material,
                               :semantics :image,
                               :data      {:src "2"}}]}]}))
    (is (= selection {:start [0 0 1] :end [0 0 1]}))))

;;-----------------------------
;; SELECTION STATE
;;-----------------------------

(deftest selection-state-border1
  (let [selection-state
        (->
          {:type
           :block,
           :content
           [{:type      :line,
             :semantics :generic-line,
             :content   [{:type    :material, :semantics :text,
                          :content "0"}
                         {:type      :material,
                          :semantics :text,
                          :format    #{:bold},
                          :content   "1"}
                         {:type    :material, :semantics :text,
                          :content "0"}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 1] :end [0 1 1]})
          (s/selection-state))]
    (is (= selection-state
           {:basic-formatting {:semantics :text :format #{:bold}},
            :block-formatting {:semantics nil, :format nil},
            :collapsed?       false,
            :current-material nil
            :current-block    nil,
            :multimaterial?   true
            :multiline?       false,
            :multiblock?      true}))))

(deftest selection-state-border
  (let [selection-state
        (->
          {:type
           :block,
           :content
           [{:type      :line,
             :semantics :generic-line,
             :content   [{:type    :material, :semantics :text,
                          :content "00"}
                         {:type      :material,
                          :semantics :text,
                          :format    #{:bold},
                          :content   "1111"}
                         {:type    :material, :semantics :text,
                          :content "22"}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 1 1] :end [0 2 0]})
          (s/selection-state))]
    (is (= selection-state
           {:basic-formatting {:semantics :text, :format #{:bold}},
            :block-formatting {:semantics nil, :format nil},
            :collapsed?       false,
            :current-material nil
            :current-block    nil,
            :multimaterial?   true
            :multiline?       false,
            :multiblock?      true}))))

(deftest selection-state-list
  (let [selection-state
        (->
          {:type
           :block,
           :content
           [{:semantics :list,
             :format    #{:unordered-list},
             :type      :block,
             :content   [{:type      :line,
                          :semantics :generic-line,
                          :content   [{:type      :material,
                                       :semantics :text,
                                       :format    #{:bold},
                                       :content   "1111"}]}
                         {:type      :line,
                          :semantics :generic-line,
                          :content   [{:type    :material, :semantics :text,
                                       :content "2222"}]}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0 1] :end [0 1 0 2]})
          (s/selection-state))]
    (is (= selection-state
           {:basic-formatting {:semantics :text, :format nil},
            :block-formatting {:semantics :list, :format #{:unordered-list}},
            :collapsed?       false,
            :current-material nil
            :current-block    nil,
            :multimaterial?   true
            :multiline?       true,
            :multiblock?      false}))))

(deftest selection-state-collapsed-list
  (let [selection-state
        (->
          {:type
           :block,
           :content
           [{:semantics :list,
             :format    #{:unordered-list},
             :type      :block,
             :content   [{:type      :line,
                          :semantics :generic-line,
                          :content   [{:type      :material,
                                       :semantics :text,
                                       :format    #{:bold},
                                       :content   "1111"}]}
                         {:type      :line,
                          :semantics :generic-line,
                          :content   [{:type    :material, :semantics :text,
                                       :content "2222"}]}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0 1] :end [0 0 0 1]})
          (s/selection-state))]
    (is (= selection-state
           {:basic-formatting {:semantics :text, :format #{:bold}},
            :block-formatting {:semantics :list, :format #{:unordered-list}},
            :collapsed?       true,
            :current-material {:type      :material,
                               :semantics :text,
                               :format    #{:bold},
                               :content   "1111"}
            :current-block    {:semantics :list, :format #{:unordered-list}, :type :block, :content [{:type :line, :semantics :generic-line, :content [{:type :material, :semantics :text, :format #{:bold}, :content "1111"}]} {:type :line, :semantics :generic-line, :content [{:type :material, :semantics :text, :content "2222"}]}]}
            :multimaterial?   false
            :multiline?       false,
            :multiblock?      false}))))

(deftest selection-state-double-list
  (let [selection-state
        (->
          {:type
           :block,
           :content
           [{:semantics :list,
             :format    #{:unordered-list},
             :type      :block,
             :content   [{:type      :line,
                          :semantics :generic-line,
                          :content   [{:type      :material,
                                       :semantics :text,
                                       :format    #{:bold},
                                       :content   "1111"}]}
                         {:type      :line,
                          :semantics :generic-line,
                          :content   [{:type    :material, :semantics :text,
                                       :content "2222"}]}]}
            {:semantics :list,
             :format    #{:unordered-list},
             :type      :block,
             :content   [{:type      :line,
                          :semantics :generic-line,
                          :content   [{:type      :material,
                                       :semantics :text,
                                       :format    #{:bold},
                                       :content   "3333"}]}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0 1] :end [1 0 0 2]})
          (s/selection-state))]
    (is (= selection-state
           {:basic-formatting {:semantics :text, :format nil},
            :block-formatting {:semantics nil, :format nil},
            :collapsed?       false,
            :current-material nil
            :current-block    nil,
            :multimaterial?   true
            :multiline?       true,
            :multiblock?      true}))))

(deftest selection-state-media-collapsed
  (let [selection-state
        (->
          {:type
           :block,
           :content
           [{:type      :line,
             :semantics :generic-line,
             :content   [{:type      :material,
                          :semantics :image,
                          :data      {:src ""}}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0] :end [0 0 0]})
          (s/selection-state))]
    (is (= selection-state
           {:basic-formatting {:semantics :image, :format nil},
            :block-formatting {:semantics nil, :format nil},
            :collapsed?       true,
            :current-material {:type :material, :semantics :image, :data {:src ""}}
            :current-block    {:type :block, :content [{:type :line, :semantics :generic-line, :content [{:type :material, :semantics :image, :data {:src ""}}]}]}
            :multimaterial?   false
            :multiline?       false,
            :multiblock?      true}))))

(deftest selection-state-media-noncollapsed
  (let [selection-state
        (->
          {:type
           :block,
           :content
           [{:type      :line,
             :semantics :generic-line,
             :content   [{:type      :material,
                          :semantics :image,
                          :data      {:src ""}}]}]}
          (wrap-conf)
          (m/open-document)
          (s/update-selection {:start [0 0 0] :end [0 0 1]})
          (s/selection-state))]
    (is (= selection-state
           {:basic-formatting {:semantics :image, :format nil},
            :block-formatting {:semantics nil, :format nil},
            :current-material {:type :material, :semantics :image, :data {:src ""}}
            :current-block    {:type :block, :content [{:type :line, :semantics :generic-line, :content [{:type :material, :semantics :image, :data {:src ""}}]}]}
            :collapsed?       false,
            :multimaterial?   false
            :multiline?       false,
            :multiblock?      true}))))

#_(require '[cryma.core.components.rte.model.tests])
#_(run-tests)

