(ns cryma.core.components.html
  (:require [hickory.render :as hr]
            [hickory.core :as hp]
            [hickory.zip :as hz]
            [clojure.string :as clostr]
            [clojure.zip :as z]
            [goog.string :as gs]))

(defn build-hiccup [el tag-builder]
  (cond
    (string? el)
    (gs/unescapeEntities el)
    (or (vector? el) (seq? el))
    (let [tag (first el)
          attrs? (second el)
          attrs? (when (map? attrs?) attrs?)
          childs (if (map? attrs?)
                   (rest (rest el))
                   (rest el))]
      (tag-builder
        tag attrs?
        (->>
          childs
          (map (fn [el] (build-hiccup el tag-builder)))
          (keep identity)
          vec)))))

(defn html->hiccup [html tag-builder]
  (->
    html
    hp/parse
    hp/as-hiccup
    first
    (build-hiccup tag-builder)))

(defn go-to-body [hz]
  (loop [z hz]
    (if (-> z z/node :tag #{:body})
      z
      (recur (z/next z)))))

(defn html->hk [html]
  (->
    html
    hp/parse
    hp/as-hickory))

(defn html->hzip [html]
  (->
    html
    html->hk
    hz/hickory-zip))

(defn html->hz-body [html]
  (->
    html
    html->hzip
    go-to-body))

(defn hzip->html [hz]
  (->
    hz
    (#(z/root (or % (html->hz-body ""))))
    hr/hickory-to-html))

(defn copy-body [html]
  (->
    html
    html->hz-body
    z/node
    hr/hickory-to-html))

(defn rightmost? [z]
  (nil? (seq (z/rights z))))

(defn leftmost? [z]
  (nil? (seq (z/lefts z))))

(defn remove-lefts [z & [remove-self?]]
  (loop [z z]
    (if (leftmost? z)
      (if remove-self? (z/remove z) z)
      (let [rmz (-> z z/left z/remove)
            new-z (z/next rmz)]
        (if (z/end? new-z)
          rmz
          (recur new-z))))))

(defn remove-rights [z & [remove-self?]]
  (let [rightmost? (rightmost? z)]
    (cond
      (and rightmost? remove-self?)
      (z/remove z)
      rightmost?
      z
      :else
      (loop [z (if remove-self? (-> z z/remove z/next) (z/right z))]
        (let [rmz (z/remove z)
              new-z (z/next rmz)]
          (if (z/end? new-z)
            rmz
            (recur new-z)))))))

(defn hz-split-body [hz-body node-predicate]
  (let [fst (z/down hz-body)
        rightmost (when fst (z/rightmost fst))]
    (loop [z fst]
      (cond
        (and z (z/node z) (node-predicate (z/node z)))
        {:before (remove-rights z)
         :after  (remove-lefts z :remove-self!)}
        (= z rightmost)
        {:before hz-body}
        :else
        (recur (z/right z))))))

(defn split-document [html node-predicate]
  (->
    html
    html->hz-body
    (hz-split-body node-predicate)
    (update :before hzip->html)
    (update :after hzip->html)))

(defn join-documents
  ([{:keys [before after]}]
   (join-documents before after))
  ([d1 d2 & ds]
   (->>
     ds
     (concat [d2])
     (reduce (fn [hz-res-body html]
               (loop [res hz-res-body z (-> html html->hz-body z/down)]
                 (let [rightmost? (when z (rightmost? z))]
                   (if (or (nil? z) (z/end? z) rightmost?)
                     (if rightmost? (z/append-child res (z/node z)) res)
                     (recur (z/append-child res (z/node z)) (z/right z))))))
             (html->hz-body d1))
     hzip->html)))

(defn zright [z]
  (cond
    (or (nil? z) (z/end? z))
    z
    (= z (z/rightmost z))
    (zright (z/up z))
    :else
    (z/right z)))

(defn get-text [html & [ignore-node-predicate]]
  (loop [z (html->hz-body html) result ""]
    (cond
      (or (nil? z) (z/end? z))
      (clostr/trim result)
      (and ignore-node-predicate (ignore-node-predicate (z/node z)))
      (recur (zright z) result)
      (and (not (z/branch? z)) (string? (z/node z)))
      (recur (z/next z) (str result (z/node z)))
      :else
      (recur (z/next z) result))))

(defn empty-html? [html & [ignore-node-predicate]]
  (loop [z (html->hz-body html) result ""]
    (cond
      (or (nil? z) (z/end? z))
      (empty? (clostr/trim result))
      (and ignore-node-predicate (ignore-node-predicate (z/node z)))
      (recur (zright z) result)
      (-> z z/node :tag #{:a :ul :ol :blockquote :img :video :iframe})
      false
      (and (not (z/branch? z)) (string? (z/node z)))
      (recur (z/next z) (str result (z/node z)))
      :else
      (recur (z/next z) result))))

(defn starts-with? [html node-predicate]
  (loop [z (html->hz-body html)]
    (cond
      (or (nil? z) (z/end? z))
      false
      (node-predicate (z/node z))
      true
      (not (z/branch? z))
      false
      :else
      (recur (z/next z)))))

(defn edit-node [html node-predicate edit-fn & args]
  (loop [z (html->hz-body html)]
    (cond
      (z/end? z)
      (hzip->html z)
      (node-predicate (z/node z))
      (if-let [node (apply edit-fn (z/node z) args)]
        (recur (z/next (z/replace z node)))
        (recur (z/next (z/remove z))))
      :else
      (recur (z/next z)))))

(defn edit-first-node [html node-predicate edit-fn & args]
  (loop [z (html->hz-body html)]
    (cond
      (or (z/end? z) (not (z/branch? z)))
      (hzip->html z)
      (node-predicate (z/node z))
      (hzip->html
        (if-let [node (apply edit-fn (z/node z) args)]
          (z/replace z node)
          (z/remove z)))
      :else
      (recur (z/next z)))))

(defn trim [html & [node-predicate]]
  (let [reducer (fn [res node]
                  (if (and
                        (empty? res)
                        (or (= :br (:tag node))
                            (and node-predicate (node-predicate node))))
                    res
                    (conj res node)))]
    (-> html str
        html->hz-body
        (z/edit update :content
                (fn [content]
                  (->> content
                       (reduce reducer [])
                       reverse
                       (reduce reducer [])
                       reverse)))
        hzip->html)))

#_(->
    "<html><head></head><body data-keks=\"{:type :block, :semantics :document}\"><br/><br/><br/><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">333</span></p><br/><br/><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">444</span></p><br/><br/><br/></body></html>"
    trim
    )


#_(->
    "<html><head></head><body data-keks=\"{:type :block, :semantics :document}\"><div data-keks=\"{:type :block, :semantics :cut}\"><p data-keks=\"{:type :line, :semantics :generic-line}\"><a>@23</a><a>@123</a></p></div></body></html>"
    #_"<html><head></head><body data-keks=\"{:type :block, :semantics :document}\"></body></html>"
    #_(starts-with? (fn [node]
                      (and (= :a (:tag node))
                           (->> node :content clostr/join string?)
                           (->> node :content clostr/join (re-find #"@\d+")))))
    (edit-first-node
      (fn [node]
        (and (= :a (:tag node))
             (->> node :content clostr/join string?)
             (->> node :content clostr/join (re-find #"@\d+"))))
      (fn [node]
        #_(assoc node :content ["@test"])))
    ;html->hz-body
    ;z/next
    ;z/node
    )

#_(->
    "<html><head></head><body data-keks=\"{:type :block, :semantics :document}\"><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">111</span></p><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">222</span></p><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">333</span></p><div data-keks=\"{:type :block, :semantics :cut}\"><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">444</span></p></div><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\"></span></p><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">555</span></p><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\"></span></p></body></html>"

    #_"<html><head></head><body data-keks=\"{:type :block, :semantics :document}\"><div data-keks=\"{:type :block, :semantics :cut}\"><p data-keks=\"{:type :line, :semantics :generic-line}\"><span data-keks=\"{:type :material, :semantics :text}\">444</span></p></div></body></html>"
    #_"<html><head></head><body data-keks=\"{:type :block, :semantics :document}\"></body></html>"

    (split-document (fn [node]
                      (-> node
                          (get-in [:attrs :data-keks])
                          (cljs.reader/read-string)
                          :semantics #{:cut})))
    (join-documents)

    ;z/down

    ;z/right z/right z/right
    ;(remove-lefts true)
    ;(remove-rights true)
    ;z/root
    ;z/rights
    ;z/node
    ;hr/hickory-to-html
    )


#_(->
    "<html><head></head><body>На написание этого текста меня подтолкнула ужасающая ситуация, сложившаяся в области интернет-коммуникаций, угрожающая перспектива развития инструментов для обмена мгновенными сообщениями, аудио-видеозвонками и надоевшие споры о том, какой же мессенджер все-таки хороший, правильный и безопасный.Последние годы конкуренция на рынке мессенджеров как никогда высока. Доступный интернет у каждого в смартфоне позволил мессенджерам стать самыми часто используемыми приложениями. Только ленивый сейчас не пишет свой мессенджер. Каждый день выходит новое приложение, обещающее совершить революцию в способах коммуникации. Доходит даже до абсурда вроде приложения Yo, позволяющего слать друг другу только одно слово.\n               У каждого мессенджера есть своя аудитория, агитирующая пользоваться именно их любимым сервисом. В итоге приходится заводить кучу учетных записей в различных сервисах и устанавливать кучу приложений, чтобы иметь возможность оперативно связаться со всеми необходимыми людьми. </body></html>"
    (html->hiccup (fn [tag attrs childs]
                    (debug "---------" tag attrs childs))))

