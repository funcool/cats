(ns cats.monad.either-spec
  #+cljs
  (:require [cljs.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.either :as either]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [clojure.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.either :as either]
            [cats.core :as m]))

(t/deftest either-monad-tests
  (t/testing "Basic either operations."
    (t/is (= 1 (either/from-either (either/right 1))))
    (t/is (= nil (either/from-either (either/left)))))

  (t/testing "Test IDeref"
    (t/is (= 1 @(either/left 1)))
    (t/is (= 1 @(either/right 1))))

  (t/testing "Test predicates"
    (let [m1 (either/right 1)]
      (t/is (either/either? m1))
      (t/is (either/right? m1))))

  (t/testing "Test fmap"
    (let [m1 (either/right 1)
          m2 (either/left)]
      (t/is (= (m/fmap inc m1) (either/right 2)))
      (t/is (= (m/fmap inc m2) (either/left)))))

  (t/testing "The first monad law: left identity"
    (t/is (= (either/right 2)
             (m/>>= (p/mreturn either/either-monad 2) either/right))))

  (t/testing "The second monad law: right identity"
    (t/is (= (either/right 2)
             (m/>>= (either/right 2) m/return))))

  (t/testing "The third monad law: associativity"
    (t/is (= (m/>>= (m/mlet [x  (either/right 2)
                             y  (either/right (inc x))]
                      (m/return y))
                    (fn [y] (either/right (inc y))))
             (m/>>= (either/right 2)
                    (fn [x] (m/>>= (either/right (inc x))
                                   (fn [y] (either/right (inc y))))))))))


(def either-vector-transformer (either/either-transformer b/vector-monad))

(t/deftest either-transformer-tests
  (t/testing "It can be combined with the effects of other monads"
    (t/is (= [(either/right 2)]
             (m/with-monad either-vector-transformer
               (m/return 2))))

    (t/is (= [(either/right 1)
              (either/right 2)
              (either/right 2)
              (either/right 3)]
             (m/with-monad either-vector-transformer
               (m/mlet [x [(either/right 0) (either/right 1)]
                        y [(either/right 1) (either/right 2)]]
                 (m/return (+ x y))))))

    (t/is (= [(either/right 1)
              (either/right 2)
              (either/right 2)
              (either/right 3)]
             (m/with-monad either-vector-transformer
               (m/mlet [x (m/lift [0 1])
                        y (m/lift [1 2])]
                 (m/return (+ x y))))))

    (t/is (= [(either/right 1)
              (either/left)
              (either/right 2)
              (either/left)]
             (m/with-monad either-vector-transformer
               (m/mlet [x [(either/right 0) (either/right 1)]
                        y [(either/right 1) (either/left)]]
                 (m/return (+ x y))))))))
