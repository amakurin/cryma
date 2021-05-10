(ns cryma.core.tools
  #?(:clj
     (:require [clojure.core.async :refer [<! <!! >! >!! put! alts! chan go go-loop]])
     :cljs
     (:require-macros [cljs.core.async.macros :refer [go go-loop]]))
  (:require
    #?(:cljs [cljs.core.async :as async :refer [<! >! put! alts! chan]])
    #?(:cljs [cljs.reader :as cljsread])
    [clojure.string :as clostr])
  #?(:clj
     (:import (java.text SimpleDateFormat)
              (java.util Date UUID))))

(defn guid []
  #?(:cljs
          (cljsread/read-string
            (str "#uuid \""
                 (clostr/replace
                   "xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx"
                   (js/RegExp. "[xy]" "g")
                   (fn [c]
                     (let [r (bit-or (* 16 (.random js/Math)) 0)]
                       (.toString (if (= c \x) r (bit-or (bit-and r 0x3) 0x8)) 16)))) "\""))
     :clj (UUID/randomUUID)))

(defn ->if [v condition f & args]
  (if condition (apply f v args) v))

(defn ensure-vector [x]
  (cond
    (vector? x) x
    (list? x) (vec x)
    :else [x]))

(defn ensure-set [x]
  (if (set? x) x #{x}))

(defn alternate-value [v alternative] (if (some? v) v alternative))

(defn korks-path [& korks]
  (->> korks flatten (map #(if (coll? %) (vec %) %)) flatten (remove nil?) vec))

(defn get-korks [m & korks]
  (if (seq korks)
    (get-in m (korks-path korks))
    m))

(defn deep-merge [& maps]
  (let [f
        (fn f [x y]
          (cond
            (nil? x) y
            (nil? y) x
            (and (map? x) (map? y)) (merge-with f x y)
            :default y))]
    (apply merge-with f maps)))

(defn apply-limit [{:keys [offset limit]} coll]
  (let [limiter (if limit (partial take limit) identity)
        offseter (if offset (partial drop offset) identity)]
    (limiter (offseter coll))))

(defn apply-order [{:keys [order]} coll]
  (if
    order
    (sort
      (->>
        order
        (reduce
          (fn [res sort-spec]
            (let [field (if (vector? sort-spec)
                          (first sort-spec) sort-spec)
                  dir (or
                        (when (vector? sort-spec)
                          (second sort-spec))
                        :desc)
                  comparer
                  (if (= dir :desc)
                    #(compare
                      (get %2 field)
                      (get %1 field))
                    #(compare
                      (get %1 field)
                      (get %2 field)))
                  prev (or res comparer)]
              (fn [v1 v2]
                (let [r (prev v1 v2)]
                  (if (= r 0)
                    (comparer v1 v2)
                    r)))))
          nil))
      coll)
    coll))

(defn prepare-value [m]
  (->>
    m
    (map
      (fn [[k v]]
        [k
         (cond
           (vector? v)
           (mapv (fn [i]
                   (if (:db/id i)
                     (select-keys i [:db/id])
                     i)) v)
           (map? v) (:db/id v)
           :else
           v)]))
    (into {})))

(defn round>int [n]
  #?(:clj  (Math/round (double n))
     :cljs (.round js/Math n)))

#?(:cljs
   (defn days-between [d-before d-after & [abs?]]
     (when (and d-before d-after)
       (let [one-day (* 24 60 60 1000)
             fract (/ (- (.getTime d-after)
                         (.getTime d-before))
                      one-day)]
         (.round js/Math
                 (if abs?
                   (.abs js/Math fract)
                   fract))))))

(defn parse-date [dt]
  (let [to-date-string
        #?(:clj  #(.format (SimpleDateFormat. "YYYY-MM-dd") %)
           :cljs #(.toDateString %))
        date-ctor
        #?(:clj  (fn [& [time]] (if time
                                  (Date. time)
                                  (Date.)))
           :cljs (fn [& [time]] (if time
                                  (js/Date. time)
                                  (js/Date.))))
        get-year #?(:clj #(+ 1900 (.getYear %)) :cljs #(.getFullYear %))
        h (.getHours dt)
        m (.getMonth dt)
        y (get-year dt)
        now (date-ctor)
        now-time (.getTime now)
        since-secs (round>int (/ (- now-time (.getTime dt)) 1000))
        months [:jan :feb :mar :apr :may :jun :jul :aug :sep :oct :nov :dec]]
    {:second        (.getSeconds dt)
     :minute        (.getMinutes dt)
     :hour          h
     :day           (.getDate dt)
     :month         (inc m)
     :month-k       (get months m)
     :year          y
     :hour2         (let [h2 (mod h 12)] (if (= 0 h2) 12 h2))
     :part          (if (>= h 12) :pm :am)
     :today?        (= (to-date-string now) (to-date-string dt))
     :yesterday?    (= (to-date-string (date-ctor (- now-time (* 24 60 60 1000))))
                       (to-date-string dt))
     :this-year?    (= y (get-year now))
     :since-seconds since-secs
     :since-minutes (round>int (/ since-secs 60))
     :since-hours   (round>int (/ since-secs (* 60 60)))}))

(defn date>str [date & [{:keys [l format-code just-now since-seconds
                                since-minutes since-hours today yesterday this-year]}]]
  (let [date-info (parse-date date)
        code (cond
               format-code format-code
               (and just-now
                    (<= (:since-seconds date-info) just-now))
               :date/just-now
               (and since-seconds
                    (<= (:since-seconds date-info) since-seconds))
               :date/since-seconds
               (and since-minutes
                    (<= (:since-minutes date-info) since-minutes))
               :date/since-minutes
               (and since-hours
                    (<= (:since-hours date-info) since-hours))
               :date/since-hours
               (and today (:today? date-info))
               :date/today
               (and yesterday (:yesterday? date-info))
               :date/yesterday
               (and this-year (:this-year? date-info))
               :date/this-year
               :else :date/generic)
        res (assoc date-info :code code)]
    (if l (l res) res)))


#_(parse-date #inst "2016-01-11T21:45:02.250-00:00")

#_(date>str
    #inst "2016-01-10T21:45:02.250-00:00"
    {:today         true
     ;:yesterday         true
     ;:this-year     true
     :just-now      100
     :since-minutes 10})