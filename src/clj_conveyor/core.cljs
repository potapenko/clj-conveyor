(ns clj-conveyor.core
  (:require [cljs.core.async :as async :refer [<! >! put! chan timeout]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(enable-console-print!)

(def request-animation-frame js/requestAnimationFrame)

(defn- await-cb [fnc & args]
  (let [port (chan)]
    (apply fnc (concat args [(fn [& a] (put! port (or a [])))]))
    port))

(def ^:private conv-counter (atom 0))
(def ^:private state (atom {}))

(defrecord ^:private Return [data])
(defrecord ^:private Bookmark [name])

(defprotocol ^:private IConveyor
  (init [this])
  (add [this cb t args])
  (pause [this t])
  (stop [this])
  (play [this])
  (clean [this])
  (clean-and-stop [this]))

(defn- ->state [id]
  (if-let [res (get @state id)]
    res
    (let [conv-state {:commands (chan) :played true}]
      (swap! state assoc id conv-state)
      conv-state)))

(defn- wait-timeout [t]
  (go
    (cond
      (number? t)          (<! (timeout t))
      (and
       (string? t)
       (re-find #"^\d+$" t)) (dotimes [x (js/parseInt t)]
       (<! (await-cb request-animation-frame)))
      :else (js/console.error "Invalid conv timeout format:" t))
    "waiting end"))

(defn- stack-call [t args]
  (go
    (<! (wait-timeout t))


    ))

(deftype Conveyor [id]
  IConveyor
  (init [this] this)
  (add [this cb t args])
  (pause [this t] this)
  (stop [this] this)
  (play [this] this)
  (clean [this] this)
  (clean-and-stop [this]
    (-> this clean stop)))

(defn ->conv []
  (-> (Conveyor. (keyword (str "conv-" (swap! conv-counter inc)))) init))

(def conv (->conv))

(comment
  (Return. "hello")
  (->conv)

  (instance? Return (Return. "hello"))

  )
