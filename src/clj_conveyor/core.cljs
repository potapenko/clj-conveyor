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
  (played? [this])
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

(defn- awake
  [id]
  (put! (-> id ->state :pause-chan) "awake!"))

(defn- wait-awake [id]
  (go
    (<! (-> id ->state :pause-chan))
    (-> id ->state update-in [id :pause-chan] (chan))
    "go to work!"))

(defn- add-bookmark [id]
  )

(defn- remove-bookmark [id]
  )

(deftype Conveyor [id]
  IConveyor
  (init [this]
    (swap! state assoc id {:stack [] :played true :pause-chan (chan)})
    (go-loop []
      (when-not (-> this played?)
        (<! (wait-awake id)))
      (if-let [command (-> id ->state :stack :first)]
        (let [[cb t args] command]
          (swap! state update-in [id :stack] rest)
          (swap! state update-in [id :stack] vec)
          (<! (wait-timeout t))
          (apply cb args))
        (<! (wait-awake id)))
      (recur))
    this)
  (add [this cb args] (add this cb nil args))
  (add [this cb t args]
    (let [empty (-> id ->state :stack empty?)]
      (add-bookmark id)
      (swap! state update-in [id :stack] conj [cb t args])
      (remove-bookmark id)
      (awake id))
    this)
  (pause [this t] this (add this t #() []))
  (played? [this] (-> id ->state :played))
  (stop [this]
    (when (played? this)
      (swap! state assoc-in [id :played] false))
    this)
  (play [this]
    (when-not (played? this)
     (swap! state assoc-in [id :played] true)
     (awake id))
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
