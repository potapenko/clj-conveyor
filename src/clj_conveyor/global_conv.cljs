(ns clj-conveyor.global-conv
  (:require [clj-conveyor.core :as conv-core]
            [cljs.core.async :as async :refer [<! >! put! chan timeout]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]]))

(def conv ^:private (conv-core/->conv))

(defn add [& args]
  (apply conv-core/add (concat [conv] args)))
(defn pause [t] (-> conv (conv-core/pause t)))
(defn start [] (-> conv conv-core/start))
(defn stop [] (-> conv conv-core/stop))
(defn stopped? [] (-> conv conv-core/stopped?))
(defn clear [] (-> conv conv-core/clear))
(defn clear-and-stop [] (-> conv conv-core/clear-and-stop))

(comment

  (do
    (add #(println "conv 1"))
    (add #(go (println "start")
              (<! (timeout 3000))
              (println "conv 2")))
    (add #(println "conv 3"))
    (add #(println "conv 3.1"))

    (add (fn []
           (add #(println "hello conv 1") 500)
           (add (fn []
                  (add (fn []
                         (add #(println "hello conv 2") 500)
                         (add #(println "hello conv 3") 500))
                       500 [3])
                  (add #(println "hello conv 4") 500)
                  (add #(println "hello conv 5") 500))
                500)
           (add #(println "hello conv 6") 500)
           (add #(println "hello conv 7") 500))))

  )
