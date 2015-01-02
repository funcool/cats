(ns cats.monad.continuation-spec
  #+cljs
  (:require [speclj.core :as s :include-macros true]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.continuation :as cont]
            [cats.monad.maybe :as maybe]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [speclj.core :as s]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.continuation :as cont]
            [cats.monad.maybe :as maybe]
            [cats.core :as m]))

(def cont-42 (cont/continuation (fn [c] (c 42))))
(def inc-cont-fn (fn [x]
                   (cont/continuation (fn [c] (c (inc x))))))

(s/describe "continuation-monad"
  (s/it "The first monad law: left identity"
    (s/should= (cont/run-cont cont-42)
               (cont/run-cont (m/with-monad cont/continuation-monad
                                (m/>>= (m/return 42)
                                       (fn [v] (cont/continuation (fn [c] c v))))))))


  (s/it "The second monad law: right identity"
    (s/should= (cont/run-cont cont-42)
               (cont/run-cont
                (m/>>= cont-42 m/return))))

  (s/it "The third monad law: associativity"
    (s/should= (cont/run-cont
                (m/>>= (m/mlet [x  cont-42
                                y  (inc-cont-fn x)]
                         (m/return y))
                       inc-cont-fn))
               (cont/run-cont
                (m/>>= cont-42
                       (fn [x] (m/>>= (cont/continuation (fn [c] (c (inc x))))
                                      inc-cont-fn))))))

  (s/it "call-cc allows the creation of resumable computations."
    (let [cc (atom nil)]
      (s/should= 44 (cont/run-cont (m/mlet [x cont-42
                                            y (cont/call-cc (fn [k]
                                                              (reset! cc k)
                                                              (k 2)))]
                                     (m/return (+ x y)))))
      (s/should= 45 (cont/run-cont (@cc 3)))
      (s/should= 46 (cont/run-cont (@cc 4))))))
