(ns cats.monad.state-spec
  #+cljs
  (:require [speclj.core :as s :include-macros true]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.data :as d]
            [cats.monad.state :as state]
            [cats.monad.maybe :as maybe]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [speclj.core :as s]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.data :as d]
            [cats.monad.state :as state]
            [cats.monad.maybe :as maybe]
            [cats.core :as m]))

(s/describe "state-monad"
  (s/it "get-state should return the identity."
    (let [computation (state/get-state)]
      (s/should= :foo (state/exec-state computation :foo))))

  (s/it "swap-state should apply function to state and return it."
    (let [computation (state/swap-state inc)]
      (s/should= 2 (state/exec-state computation 1))))

  (s/it "put-state should override the state"
    (let [computation (state/put-state 42)]
      (s/should= 42
             (state/exec-state computation 0))))

  (s/it "State monad composition with mlet should return state"
    (let [res (m/mlet [s (state/get-state)]
                (m/return (inc s)))]
      (s/should (state/state? res))
      (let [res (state/run-state res 1)]
        (s/should (d/pair? res))
        (s/should= 2 (first res))
        (s/should= 1 (second res)))))

  (s/it "State monad computations can be mapped over"
    (let [res (m/mlet [s (state/get-state)]
                (m/return (inc s)))
          res (m/fmap inc res)]
      (s/should (state/state? res))
      (let [res (state/run-state res 1)]
        (s/should (d/pair? res))
        (s/should= 3 (first res))
        (s/should= 1 (second res))))))


(def maybe-state-monad (state/state-transformer maybe/maybe-monad))

(s/describe "state-transformer"
  (s/it "get-state should return the identity wrapped in the inner monad's context."
    (s/should= (maybe/just (d/pair :foo :foo))
               (m/with-monad maybe-state-monad
                 (state/run-state (state/get-state) :foo))))

  (s/it "swap-state should should apply function to state and return it."
    (s/should= (maybe/just (d/pair 1 2))
               (m/with-monad maybe-state-monad
                 (state/run-state (state/swap-state inc) 1))))

  (s/it "put-state should override the state"
    (s/should= (maybe/just (d/pair 0 42))
               (m/with-monad maybe-state-monad
                 (state/run-state (state/put-state 42) 0))))

  (s/it "State monad composition with mlet should return state"
    (let [res (m/mlet [s (state/get-state)]
                (m/return (inc s)))]
      (s/should (state/state? res))
      (let [mres (m/with-monad maybe-state-monad
                   (state/run-state res 1))
            res  (maybe/from-maybe mres)]
        (s/should (maybe/maybe? mres))
        (s/should (d/pair? res))
        (s/should= 2 (first res))
        (s/should= 1 (second res)))))

  (s/it "State monad computations can be mapped over"
    (let [res (m/mlet [s (state/get-state)]
                (m/return (inc s)))]
      (s/should (state/state? res))
      (let [mres (m/with-monad maybe-state-monad
                   (state/run-state (m/fmap inc res) 1))
            res (maybe/from-maybe mres)]
        (s/should (maybe/maybe? mres))
        (s/should (d/pair? res))
        (s/should= 3 (first res))
        (s/should= 1 (second res)))))

  (s/it "Inner monad values can be lifted into the transformer"
    (let [lifted-just (m/mlet [s (state/get-state)
                               i (m/lift (maybe/just 3))]
                        (m/return (+ i s)))
          lifted-nothing (m/mlet [s (state/get-state)
                                  i (m/lift (maybe/nothing))]
                           (m/return (+ s i)))]
      (s/should= (maybe/just (d/pair 45 42))
                 (m/with-monad maybe-state-monad
                   (state/run-state lifted-just 42)))
      (s/should= (maybe/nothing)
                 (m/with-monad maybe-state-monad
                   (state/run-state lifted-nothing nil))))))
