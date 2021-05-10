(ns cryma.core.components.delayed
  (:require-macros
    [cljs.core.async.macros :refer [go]])
  (:require [cljs.core.async :as async :refer [put! <! >! alts!]]
            [cryma.core.api :as api]))

(defn delayed-ctor
  "Creates delayed object to make sliding-buf analogue for same sequential actions
  timeot-ms - action execution timeout"
  [timeout-ms]
  (atom {:timeout-ms timeout-ms :sliding-buf nil
         :prolong-ch nil :force-ch nil}))

(defn delayed-action
  "Executes supplied action on supplied delayed object after timeout.
  Actions are placing in sliding buffer of size 1 and restarting timeout.
  Returns a channel that will receive action result or will be closed, when action replaced before timeout expired.
  delayed - delayed object
  action - fn of zero params, returning async/chan."
  [delayed action]
  (let [{:keys [timeout-ms sliding-buf]}
        (swap! delayed update :sliding-buf
               (fn [v]
                 {:ch     (async/chan 1)
                  :action action
                  :prev?  v}))]
    (if-let [prev (:prev? sliding-buf)]
      (put! (:prolong-ch @delayed) prev)
      (let [{:keys [prolong-ch force-ch]}
            (swap! delayed
                   merge {:prolong-ch (async/chan 1)
                          :force-ch   (async/chan 1)})]
        (go
          (loop []
            (let [[v port] (alts! [prolong-ch force-ch
                                   (async/timeout timeout-ms)])]
              (if (not= port prolong-ch)
                (let [{:keys [ch action]} (:sliding-buf @delayed)]
                  (swap! delayed
                         (fn [dc]
                           (async/close! (:prolong-ch dc))
                           (async/close! (:force-ch dc))
                           (select-keys dc [:timeout-ms])))
                  (if (and (= port force-ch) (= v :cancel!))
                    (async/close! ch)
                    (async/pipe (action) ch)))
                (do
                  (async/close! (:ch v))
                  (recur))))))))
    (:ch sliding-buf)))

(defn force-delayed [delayed]
  (when-let [force-ch (:force-ch @delayed)]
    (put! force-ch :force!)))

(defn cancel-delayed [delayed]
  (when-let [force-ch (:force-ch @delayed)]
    (put! force-ch :cancel!)))

(defn delayed-fn-call [delayed f]
  (delayed-action delayed (fn [] (let [ch (async/chan)]
                                   (async/close! ch)
                                   (f) ch))))

(defn delayed-async-call [delayed core req]
  (delayed-action delayed (fn [] (api/call-async core req))))

(defn delayed-pub-event [delayed core ev]
  (delayed-fn-call delayed
                   (fn [] (api/pub-event core ev))))
