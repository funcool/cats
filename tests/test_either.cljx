(ns test-either
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.protocols :as p]
            [cats.builtin :as b]
            [cats.monad.either :as either])
   #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-monad)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet with-monad]]
            [cats.protocols :as p]
            [cats.builtin :as b]
            [cats.monad.either :as either]))


(deftest test-either-monad
  (testing "Basic either operations."
    (is (= 1 (either/from-either (either/right 1))))
    (is (= nil (either/from-either (either/left)))))

  (testing "Test predicates"
    (let [m1 (either/right 1)]
      (is (either/either? m1))
      (is (either/right? m1))))

  (testing "Test fmap"
    (let [m1 (either/right 1)
          m2 (either/left)]
      (is (= (m/fmap inc m1) (either/right 2)))
      (is (= (m/fmap inc m2) (either/left)))))

  (testing "The first monad law: left identity"
    (is (= (either/right 2)
           (m/>>= (p/mreturn either/either-monad 2) either/right))))

  (testing "The second monad law: right identity"
    (is (= (either/right 2)
           (m/>>= (either/right 2) m/return))))

  (testing "The third monad law: associativity"
    (is (= (m/>>= (mlet [x  (either/right 2)
                         y  (either/right (inc x))]
                        (m/return y))
                  (fn [y] (either/right (inc y))))
           (m/>>= (either/right 2)
                  (fn [x] (m/>>= (either/right (inc x))
                                (fn [y] (either/right (inc y))))))))))

(deftest test-either-trans
  (let [either-vector-trans (either/either-trans b/vector-monad)]
    (testing "It can be combined with the effects of other monads"
      (is (= [(either/right 2)]
             (with-monad either-vector-trans
               (m/return 2))))

      (is (= [(either/right 1) (either/right 2) (either/right 2) (either/right 3)]
             (with-monad either-vector-trans
               (mlet [x [(either/right 0) (either/right 1)]
                      y [(either/right 1) (either/right 2)]]
                     (m/return (+ x y))))))

      (is (= [(either/right 1) (either/right 2) (either/right 2) (either/right 3)]
             (with-monad either-vector-trans
               (mlet [x (m/lift [0 1])
                      y (m/lift [1 2])]
                     (m/return (+ x y))))))

      (is (= [(either/right 1) (either/left) (either/right 2) (either/left)]
             (with-monad either-vector-trans
               (mlet [x [(either/right 0) (either/right 1)]
                      y [(either/right 1) (either/left)]]
                     (m/return (+ x y)))))))))
