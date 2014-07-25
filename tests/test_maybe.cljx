(ns test-maybe
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.protocols :as p]
            [cats.builtin :as b]
            [cats.monad.maybe :as maybe])
   #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-monad)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet with-monad]]
            [cats.protocols :as p]
            [cats.builtin :as b]
            [cats.monad.maybe :as maybe]))


(deftest test-maybe-monad
  (testing "Basic maybe operations."
    (is (= 1 (maybe/from-maybe (maybe/just 1))))
    (is (= nil (maybe/from-maybe (maybe/nothing)))))

  (testing "get-value function"
    (is (= (p/get-value (maybe/just 1)) 1))
    (is (= (p/get-value (maybe/nothing)) nil)))

  (testing "Test predicates"
    (let [m1 (maybe/just 1)]
      (is (maybe/maybe? m1))
      (is (maybe/just? m1))))

  (testing "Test fmap"
    (let [m1 (maybe/just 1)
          m2 (maybe/nothing)]
      (is (= (m/fmap inc m1) (maybe/just 2)))
      (is (= (m/fmap inc m2) (maybe/nothing)))))

  (testing "The first monad law: left identity"
    (is (= (maybe/just 2)
           (m/>>= (p/mreturn maybe/maybe-monad 2) maybe/just))))

  (testing "The second monad law: right identity"
    (is (= (maybe/just 2)
           (m/>>= (maybe/just 2) m/return))))

  (testing "The third monad law: associativity"
    (is (= (m/>>= (mlet [x  (maybe/just 2)
                         y  (maybe/just (inc x))]
                    (m/return y))
                  (fn [y] (maybe/just (inc y))))
           (m/>>= (maybe/just 2)
                  (fn [x] (m/>>= (maybe/just (inc x))
                                 (fn [y] (maybe/just (inc y))))))))))


(deftest test-maybe-trans
  (let [maybe-vector-trans (maybe/maybe-trans b/vector-monad)]
    (testing "It can be combined with the effects of other monads"
      (is (= [(maybe/just 2)]
             (with-monad maybe-vector-trans
               (m/return 2))))

      (is (= [(maybe/just 1) (maybe/just 2) (maybe/just 2) (maybe/just 3)]
             (with-monad maybe-vector-trans
               (mlet [x [(maybe/just 0) (maybe/just 1)]
                      y [(maybe/just 1) (maybe/just 2)]]
                     (m/return (+ x y))))))

      (is (= [(maybe/just 1) (maybe/just 2) (maybe/just 2) (maybe/just 3)]
             (with-monad maybe-vector-trans
               (mlet [x (m/lift [0 1])
                      y (m/lift [1 2])]
                     (m/return (+ x y))))))

      (is (= [(maybe/just 1) (maybe/nothing) (maybe/just 2) (maybe/nothing)]
             (with-monad maybe-vector-trans
               (mlet [x [(maybe/just 0) (maybe/just 1)]
                      y [(maybe/just 1) (maybe/nothing)]]
                     (m/return (+ x y)))))))))
