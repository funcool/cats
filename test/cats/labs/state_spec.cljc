(ns cats.labs.state-spec
  #?(:cljs
     (:require [cljs.test :as t]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.data :as d]
               [cats.labs.state :as state]
               [cats.monad.maybe :as maybe]
               [cats.context :as ctx :include-macros true]
               [cats.core :as m :include-macros true])
     :clj
     (:require [clojure.test :as t]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.data :as d]
               [cats.labs.state :as state]
               [cats.monad.maybe :as maybe]
               [cats.context :as ctx]
               [cats.core :as m])))

(t/deftest get-state-should-return-the-identity-test
  (let [computation (state/get-state)]
    (t/is (= :foo (state/exec-state computation :foo)))))

(t/deftest swap-state-should-apply-function-to-state-and-return-it-test
  (let [computation (state/swap-state inc)]
    (t/is (= 2 (state/exec-state computation 1)))))

(t/deftest put-state-should-override-the-state
  (let [computation (state/put-state 42)]
    (t/is (= 42
             (state/exec-state computation 0)))))

(t/deftest state-monad-in-mlet-composition
  ;; State monad composition with mlet should return state
  (let [res (m/mlet [s (state/get-state)]
              (m/return (inc s)))]
    (t/is (state/state? res))
    (let [res (state/run-state res 1)]
      (t/is (d/pair? res))
      (t/is (= 2 (first res)))
      (t/is (= 1 (second res))))))

(t/deftest state-monad-can-be-mapped-over
  ;; State monad computations can be mapped over
  (let [res (m/mlet [s (state/get-state)]
              (m/return (inc s)))
        res (m/fmap inc res)]
    (t/is (state/state? res))
    (let [res (state/run-state res 1)]
      (t/is (d/pair? res))
      (t/is (= 3 (first res)))
      (t/is (= 1 (second res))))))


(def state-maybe-t
  (state/state-transformer maybe/context))

(t/deftest state-transformer-tests
  ;; get-state should return the identity wrapped in the inner monad's context.
  (t/is (= (maybe/just (d/pair :foo :foo))
           (ctx/with-context state-maybe-t
             (state/run-state (state/get-state) :foo))))

  ;; swap-state should should apply function to state and return it.
  (t/is (= (maybe/just (d/pair 1 2))
           (ctx/with-context state-maybe-t
             (state/run-state (state/swap-state inc) 1))))

  ;; put-state should override the state
  (t/is (= (maybe/just (d/pair 0 42))
           (ctx/with-context state-maybe-t
             (state/run-state (state/put-state 42) 0))))

  ;; State monad composition with mlet should return state
  (let [res (m/mlet [s (state/get-state)]
              (m/return (inc s)))]
    (t/is (state/state? res))
    (let [mres (ctx/with-context state-maybe-t
                 (state/run-state res 1))
          res  (maybe/from-maybe mres)]
      (t/is (maybe/maybe? mres))
      (t/is (d/pair? res))
      (t/is (= 2 (first res)))
      (t/is (= 1 (second res)))))

  ;; State monad computations can be mapped over
  (let [res (m/mlet [s (state/get-state)]
              (m/return (inc s)))]
    (t/is (state/state? res))
    (let [mres (ctx/with-context state-maybe-t
                 (state/run-state (m/fmap inc res) 1))
          res (maybe/from-maybe mres)]
      (t/is (maybe/maybe? mres))
      (t/is (d/pair? res))
      (t/is (= 3 (first res)))
      (t/is (= 1 (second res)))))

  ;; Inner monad values can be lifted into the transformer
  (let [lifted-just (m/mlet [s (state/get-state)
                             i (m/lift (maybe/just 3))]
                      (m/return (+ i s)))
        lifted-nothing (m/mlet [s (state/get-state)
                                i (m/lift (maybe/nothing))]
                         (m/return (+ s i)))]
    (t/is (= (maybe/just (d/pair 45 42))
             (ctx/with-context state-maybe-t
               (state/run-state lifted-just 42))))
    (t/is (= (maybe/nothing)
             (ctx/with-context state-maybe-t
               (state/run-state lifted-nothing nil))))))
