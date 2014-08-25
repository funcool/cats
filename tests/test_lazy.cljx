(ns test-lazy
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.monad.lazy :as lazy]
            [cats.core :as m])
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-monad)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.monad.lazy :as lazy]
            [cats.core :as m :refer [mlet with-monad]]))

(deftest lazy-monad
  (testing "fmap function"
    (let [state (atom 0)
          fv1   (delay (swap! state inc))
          fv2   (m/fmap inc fv1)]
      (is (delay? fv2))
      (is (= @state 0))
      (is (= @fv2 2))
      (is (= @state 1))))

  (testing "pure function"
    (let [fv1 (m/pure lazy/lazy-monad 10)]
      (is (delay? fv1))
      (is (= @fv1 10))))

  (testing "return function"
    (let [fv1 (m/return lazy/lazy-monad 10)]
      (is (delay? fv1))
      (is (= @fv1 10))))

  (testing "fapply function"
    (let [fv1 (delay (fn [x] (inc x)))
          fv2 (m/fapply fv1 (maybe/just 1))]
      (is (delay? fv2))
      (is (= @fv2 2)))

    (let [state (atom 0)
          fv1   (delay (fn [x] (inc x)))
          fv2   (delay (swap! state inc) 1)
          fv3   (m/fapply fv1 fv2)]
      (is (delay? fv3))
      (is (= @state 0))
      (is (= @fv3 2))
      (is (= @state 1))))

  (testing "bind function"
    (let [state (atom 0)
          fv1   (mlet [x (delay 1)
                       y (delay 2)]
                  (swap! state inc)
                  (m/return (+ x y)))]
      (is (= @state 0))
      (is (delay? fv1))
      (is (= @fv1 3))
      (is (= @state 1))))
)

(deftest test-lazy-trans
  (let [monad-t (lazy/lazy-trans maybe/maybe-monad)]
    (testing "It can be combined with the effects of other monads"
      (let [mres (with-monad monad-t
                   (m/return 2))]
        (is (delay? mres))
        (is (maybe/just? @mres))))))
