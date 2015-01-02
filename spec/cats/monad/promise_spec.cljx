(ns cats.monad.promise-spec
  ;; #+cljs
  ;; (:require [speclj.core :as s :include-macros true]
  ;;           [cats.builtin :as b]
  ;;           [cats.protocols :as p]
  ;;           [cats.monad.promise :as prom]
  ;;           [cats.core :as m :include-macros true])

  #+clj
  (:require [clojure.core.async :refer [go chan put! take! <! >! <!! >!!]]
            [speclj.core :as s]
            [cats.builtin :as b]
            [cats.protocols :as pt]
            [cats.monad.promise :as p]
            [cats.core :as m]))
#+clj
(s/describe "basic promise"
  (s/it "Should be resolved"
    (s/should (p/resolved? (p/promise 1))))

  (s/it "Should be not resolved"
    (s/should-not (p/resolved? (p/promise))))

  (s/it "Should be chained."
    (let [p1 (p/then (p/promise 1)
                     (fn [v]
                       (inc v)))]
      (s/should= 2 (<!! p1))))

  (s/it "Should be chained multiple times"
    (let [p1 (-> (p/promise 1)
                 (p/then #(inc %))
                 (p/then #(inc %))
                 (p/then #(inc %))
                 (p/then #(inc %))
                 (p/then #(inc %)))]
      (s/should= 6 (<!! p1))))

  (s/it "Resolve promise with put!"
    (let [p1 (p/promise)]
      (put! p1 1)
      (s/should= 1 (<!! p1))))

  (s/it "Resolve promise twice"
    (let [p1 (p/promise)]
      (put! p1 1)
      (s/should= 1 (<!! p1))
      (s/should= 1 (<!! p1))
      (s/should= 1 (<!! p1))
      (s/should= 1 (<!! p1))))

  (s/it "Then with take!"
    (let [c1 (chan 1)
          p1 (p/promise 1)]
      (take! p1 (fn [v] (put! c1 v)))
      (s/should= 1 (<!! c1))))
)

#+clj
(s/describe "promise-monad"
  (s/it "promise as functor 1"
    (let [p1 (p/promise 1)]
      (s/should= 2 (<!! (m/fmap inc p1)))))

  (s/it "promise as functor 2"
    (let [p1 (p/promise)]
      (go
        (>!! p1 1))
      (s/should= 2 (<!! (m/fmap inc p1)))))

  (s/it "promise as monad 1"
    (let [p1 (p/promise 1)]
      (s/should= 2 (<!! (m/>>= p1 (fn [x] (m/return (inc x))))))))

  (s/it "promise as monad 2"
    (let [p1 (m/mlet [x (p/promise 1)
                      y (p/promise 1)
                      z (p/promise 2)]
               (m/return (+ x y z)))]
      (s/should= 4 (<!! p1))))

  ;; TODO: add monad laws tests
)

#+clj
(s/run-specs)

