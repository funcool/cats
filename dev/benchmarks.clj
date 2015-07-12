(ns benchmarks
  (:use cats.builtin)
  (:require [criterium.core :as c]
            [cats.core :as m])
  (:gen-class))

(def range1 (into [] (range 100)))
(def range2 (into [] (range 200)))

(defn bench-for-loop
  []
  (letfn [(using-for-loop []
            (doall
             (for [x range1
                   y range2]
               [x y])))
          (using-cats-mlet []
            (m/mlet [x range1
                     y range2]
              [x y]))]
    (println "***************** FOR LOOP ********************")
    (c/with-progress-reporting (c/bench (using-for-loop) :verbose))
    (println "***************** MLET ********************")
    (c/with-progress-reporting (c/bench (using-cats-mlet) :verbose))))

(defn -main
  [& args]
  (bench-for-loop))

