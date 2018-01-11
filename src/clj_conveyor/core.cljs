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

(defn- add-bookmark [id]
  (swap! state update-in [id :stack] conj :bookmark))

(defn- remove-bookmark [id]
  (loop [[v & t] (-> id ->state :stack)
         top     '()]
    (when v
      (if (= v :bookmark)
        (let [stack (concat t top)]
          (swap! state assoc-in [id :stack] (vec stack)))
        (recur t (concat top [v]))))))

(deftype Conveyor [id]
  IConveyor
  (init [this]
    (swap! state assoc id {:stack [] :played true :pause-chan (chan)})
    (go-loop []
      (when-not (-> this played?)
        (<! (wait-awake id)))
      (if-let [command (-> id ->state :stack first)]
        (let [[cb t args] command]
          (swap! state update-in [id :stack] rest)
          (swap! state update-in [id :stack] vec)
          (add-bookmark id)
          (apply cb args)
          (remove-bookmark id)
          (<! (wait-timeout t)))
        (<! (wait-awake id)))
      (recur))
    this)
  (add [this cb args] (add this cb nil args))
  (add [this cb t args]
    (swap! state update-in [id :stack] conj [cb t args])
    (awake id)
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
  (clean [this] (init id)
    (swap! state update-in [id :stack] empty)
    this)
  (clean-and-stop [this]
    (-> this clean stop)))

(defn ->conv []
  (-> (Conveyor. (keyword (str "conv-" (swap! conv-counter inc)))) init))

(def conv (->conv))

(comment
  (Return. "hello")
  (->conv)

  @state

  (-> conv (add (fn []
                  (-> conv (add #(println "hello conv 1") 500 [1]))
                  (-> conv (add (fn []
                                  (-> conv (add #(println "hello conv 2") 500 [2]))
                                  (-> conv (add #(println "hello conv 3") 500 [2])))
                                500 [3]))
                  (-> conv (add #(println "hello conv 4") 500 [4]))
                  (-> conv (add #(println "hello conv 5") 500 [5]))
                  (println "hello conv 0")) 50 [0]))

  (-> conv (add #(println "hello conv 1!!!!!!!!!!!!") 50 []))

  (instance? Return (Return. "hello"))

  )
