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
  (add [this cb args] [this cb t args])
  (pause [this t])
  (stop [this])
  (play [this])
  (clean [this])
  (clean-and-stop [this]))

(defn- wait-timeout [t]
  (go
    (cond
      (number? t)            (<! (timeout t))
      (and
       (string? t)
       (re-find #"^\d+$" t)) (dotimes [x (js/parseInt t)]
                               (<! (await-cb request-animation-frame)))
      (nil? t)               :nothing
      :else                  (js/console.error "Invalid conv timeout format:" t))
    "waiting end"))

(defn- ->state [id]
  (get @state id))

(defn- get-last-stack [id]
  (-> (->state id) :stack last))

(deftype Conveyor [id]
  IConveyor
  (init [this]
    (swap! state assoc id {:stack [(chan)] :played true :counter 0 :pause-chan (chan)})
    (go-loop []
      (let [[cb t args] (<! (get-last-stack id))]
        (<! (wait-timeout t))
        (let [c (-> id ->state :counter)]
          (swap! state update-in [id :stack] [] [(chan)])
          (apply cb args)
          (when (not= c (-> id ->state :counter))

            )))
      (recur))
    this)
  (add [this cb args] (add this cb nil args))
  (add [this cb t args]
    (swap! state update-in [id :counter] inc)
    (put! (get-last-stack) [cb t args])
    this)
  (pause [this t] this (add this t #() []))
  (stop [this]
    (swap! state assoc-in [id :played] false) this)
  (play [this]
    (swap! state assoc-in [id :played] true)
    (put! (-> id ->state :pause-chan) [cb t args])
    this)
  (clean [this] (init id) this)
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
