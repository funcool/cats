(ns test-state
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.monad.state :as state])
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet]]
            [cats.monad.state :as state]))


(deftest test-state-monad
  (testing "get-state should return the identity."
    (let [computation (state/get-state)]
      (is (= :foo (state/exec-state computation :foo)))))

  (testing "swap-state should should apply function to state and return it."
    (let [computation (state/swap-state inc)]
      (is (= 2 (state/exec-state computation 1)))))

  (testing "State monad composition with mlet should return state"
    (let [res (mlet [s (state/get-state)]
                (m/return (inc s)))]
      (is (state/state? res))
      (let [res (state/run-state res 1)]
        (is (state/pair? res))
        (is (= 2 (first res)))
        (is (= 1 (second res)))))))
