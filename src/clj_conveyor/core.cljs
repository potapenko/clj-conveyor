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

(defprotocol ^:private IConveyor
  (init [this])
  (add [this cb] [this cb args] [this cb t args])
  (pause [this t])
  (start [this])
  (stop [this])
  (stopped? [this])
  (clear [this])
  (clear-and-stop [this]))

(defn- wait-timeout [t]
  (go
    (cond
      (number? t)            (when (pos? t) (<! (timeout t)))
      (and
       (string? t)
       (re-find #"^\d+$" t)) (dotimes [x (js/parseInt t)]
                               (<! (await-cb request-animation-frame)))
      (nil? t)               :nothing
      :else                  (js/console.error "Invalid conv timeout format:" t))
    "waiting end"))

(defn- ->state [id]
  (get @state id))

(defn- awake [id]
  (put! (-> id ->state :pause-chan) "awake!"))

(defn- wait-awake [id]
  (go
    (<! (-> id ->state :pause-chan))
    (swap! state assoc-in [id :pause-chan] (chan))
    "go to work!"))

(deftype Conveyor [id]
  IConveyor
  (init [this]
    (swap! state assoc id {:stack [] :stopped false :pause-chan (chan)})
    (go-loop []
      (when (-> this stopped?)
        (<! (wait-awake id)))
      (if-let [command (-> id ->state :stack first)]
        (let [[cb t args] command]
          (let [stack (-> id ->state :stack rest)]
            (swap! state assoc-in [id :stack] [])
            (apply cb args)
            (swap! state update-in [id :stack] concat stack))
          (<! (wait-timeout t)))
        (<! (wait-awake id)))
      (recur))
    this)
  (add [this cb] (add this cb []))
  (add [this cb args] (add this cb nil args))
  (add [this cb t args]
    (swap! state update-in [id :stack] conj [cb t args])
    (awake id)
    this)
  (pause [this t] this (add this t #() []))
  (stopped? [this] (-> id ->state :stopped))
  (start [this]
    (when (stopped? this)
      (swap! state assoc-in [id :stopped] false))
    this)
  (stop [this]
    (when-not (stopped? this)
     (swap! state assoc-in [id :stopped] true)
     (awake id))
    this)
  (clear [this] (init id)
    (swap! state update-in [id :stack] empty)
    this)
  (clear-and-stop [this]
    (-> this clear stop)))

(defn ->conv []
  (-> (Conveyor. (keyword (str "conv-" (swap! conv-counter inc)))) init))

(def conv (->conv))

(comment
  (-> conv (add (fn []
             (-> conv (add #(println "hello conv 1") 500 []))
             (-> conv (add (fn []
                             (-> conv (add (fn []
                                             (-> conv (add #(println "hello conv 2") 500 []))
                                             (-> conv (add #(println "hello conv 3") 500 [])))
                                           500 [3]))
                             (-> conv (add #(println "hello conv 4") 500 []))
                             (-> conv (add #(println "hello conv 5") 500 [])))
                           500 []))
             (-> conv (add #(println "hello conv 6") 500 []))
             (-> conv (add #(println "hello conv 7") 500 [])))))

  )
