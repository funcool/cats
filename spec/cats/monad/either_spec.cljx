(ns cats.monad.either-spec
  #+cljs
  (:require [speclj.core :as s :include-macros true]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.either :as either]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [speclj.core :as s]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.either :as either]
            [cats.core :as m]))

(s/describe "either-monad"
  (s/it "Basic either operations."
    (s/should= 1 (either/from-either (either/right 1)))
    (s/should= nil (either/from-either (either/left))))

  (s/it "Test IDeref"
    (s/should= 1 @(either/left 1))
    (s/should= 1 @(either/right 1)))

  (s/it "Test predicates"
    (let [m1 (either/right 1)]
      (s/should (either/either? m1))
      (s/should (either/right? m1))))

  (s/it "Test fmap"
    (let [m1 (either/right 1)
          m2 (either/left)]
      (s/should= (m/fmap inc m1) (either/right 2))
      (s/should= (m/fmap inc m2) (either/left))))

  (s/it "The first monad law: left identity"
    (s/should= (either/right 2)
               (m/>>= (p/mreturn either/either-monad 2) either/right)))

  (s/it "The second monad law: right identity"
    (s/should= (either/right 2)
               (m/>>= (either/right 2) m/return)))

  (s/it "The third monad law: associativity"
    (s/should= (m/>>= (m/mlet [x  (either/right 2)
                               y  (either/right (inc x))]
                        (m/return y))
                      (fn [y] (either/right (inc y))))
               (m/>>= (either/right 2)
                      (fn [x] (m/>>= (either/right (inc x))
                                     (fn [y] (either/right (inc y)))))))))


(def either-vector-transformer (either/either-transformer b/vector-monad))

(s/describe "either-transformer"
  (s/it "It can be combined with the effects of other monads"
    (s/should= [(either/right 2)]
               (m/with-monad either-vector-transformer
                 (m/return 2)))

    (s/should= [(either/right 1)
                (either/right 2)
                (either/right 2)
                (either/right 3)]
               (m/with-monad either-vector-transformer
                 (m/mlet [x [(either/right 0) (either/right 1)]
                          y [(either/right 1) (either/right 2)]]
                   (m/return (+ x y)))))

    (s/should= [(either/right 1)
                (either/right 2)
                (either/right 2)
                (either/right 3)]
               (m/with-monad either-vector-transformer
                 (m/mlet [x (m/lift [0 1])
                          y (m/lift [1 2])]
                   (m/return (+ x y)))))

    (s/should= [(either/right 1)
                (either/left)
                (either/right 2)
                (either/left)]
               (m/with-monad either-vector-transformer
                 (m/mlet [x [(either/right 0) (either/right 1)]
                          y [(either/right 1) (either/left)]]
                   (m/return (+ x y)))))))
