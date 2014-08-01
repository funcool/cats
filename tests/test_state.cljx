(ns test-state
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.data :as d]
            [cats.monad.maybe :as maybe]
            [cats.monad.state :as state])
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-monad)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet with-monad]]
            [cats.data :as d]
            [cats.monad.maybe :as maybe]
            [cats.monad.state :as state]))

(deftest test-state-monad
  (testing "get-state should return the identity."
    (let [computation (state/get-state)]
      (is (= :foo (state/exec-state computation :foo)))))

  (testing "swap-state should apply function to state and return it."
    (let [computation (state/swap-state inc)]
      (is (= 2 (state/exec-state computation 1)))))

  (testing "put-state should override the state"
    (let [computation (state/put-state 42)]
      (is (= 42
             (state/exec-state computation 0)))))

  (testing "State monad composition with mlet should return state"
    (let [res (mlet [s (state/get-state)]
                (m/return (inc s)))]
      (is (state/state? res))
      (let [res (state/run-state res 1)]
        (is (d/pair? res))
        (is (= 2 (first res)))
        (is (= 1 (second res))))))

  (testing "State monad computations can be mapped over"
    (let [res (mlet [s (state/get-state)]
                (m/return (inc s)))
          res (m/fmap inc res)]
      (is (state/state? res))
      (let [res (state/run-state res 1)]
        (is (d/pair? res))
        (is (= 3 (first res)))
        (is (= 1 (second res)))))))

(deftest test-state-trans
  (let [maybe-state-monad (state/state-trans maybe/maybe-monad)]
    (testing "get-state should return the identity wrapped in the inner monad's context."
      (is (= (maybe/just (d/pair :foo :foo))
             (with-monad maybe-state-monad
               (state/run-state (state/get-state) :foo)))))

    (testing "swap-state should should apply function to state and return it."
      (is (= (maybe/just (d/pair 1 2))
             (with-monad maybe-state-monad
               (state/run-state (state/swap-state inc) 1)))))

    (testing "put-state should override the state"
      (is (= (maybe/just (d/pair 0 42))
             (with-monad maybe-state-monad
               (state/run-state (state/put-state 42) 0)))))

    (testing "State monad composition with mlet should return state"
      (let [res (mlet [s (state/get-state)]
                  (m/return (inc s)))]
        (is (state/state? res))
        (let [mres (with-monad maybe-state-monad
                     (state/run-state res 1))
              res  (maybe/from-maybe mres)]
          (is (maybe/maybe? mres))
          (is (d/pair? res))
          (is (= 2 (first res)))
          (is (= 1 (second res))))))

    (testing "State monad computations can be mapped over"
      (let [res (mlet [s (state/get-state)]
                  (m/return (inc s)))]
        (is (state/state? res))
        (let [mres (with-monad maybe-state-monad

                     (state/run-state (m/fmap inc res) 1))
             res (maybe/from-maybe mres)]
          (is (maybe/maybe? mres))
          (is (d/pair? res))
          (is (= 3 (first res)))
          (is (= 1 (second res))))))

    (testing "Inner monad values can be lifted into the transformer"
      (let [lifted-just (mlet [s (state/get-state)
                               i (m/lift (maybe/just 3))]
                          (m/return (+ i s)))
            lifted-nothing (mlet [s (state/get-state)
                                  i (m/lift (maybe/nothing))]
                             (m/return (+ s i)))]
        (is (= (maybe/just (d/pair 45 42))
               (with-monad maybe-state-monad
                 (state/run-state lifted-just 42))))
        (is (= (maybe/nothing)
               (with-monad maybe-state-monad
                 (state/run-state lifted-nothing nil))))))))
