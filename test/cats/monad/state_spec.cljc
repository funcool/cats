(ns cats.monad.state-spec
  #?@(:clj
       [(:require
         [cats.context :as ctx]
         [cats.core :as m]
         [cats.data :as d]
         [cats.monad.state :as state]
         [clojure.test :as t])]
       :cljs
       [(:require
         [cats.context :as ctx :include-macros true]
         [cats.core :as m :include-macros true]
         [cats.data :as d]
         [cats.monad.state :as state]
         [cljs.test :as t])]))

(def postincrement
  (m/mlet [x (state/get)
           _ (state/put (+ x 1))]
          (m/return x)))

(t/deftest state-monad-tests

  (t/testing "state"
    (let [mstate (state/state (fn [st] (d/pair "foo" (* 2 st))))]
      (t/is (= (state/state? mstate) true))
      (t/is (=  (state/run mstate 2) (d/pair "foo" 4)))))

  (t/testing "monad operations"
    (t/is (= (state/run (ctx/with-context state/context (m/return 1)) 0) (d/pair 1 0)))
    (let [mstate1 (state/get)
          func (fn [value] (state/state (fn [st] [(+ 2 st) (+ value st)])))
          mstate2 (m/bind mstate1 func)]
      (t/is (= (state/state? mstate2) true))
      (t/is (state/run mstate2 1) [3 2])))

  (t/testing "put"
    (let [put-hello (state/put "hello")]
      (t/is (= (state/run put-hello "x") (d/pair "x" "hello")))))

  (t/testing "get"
    (t/is (= (state/run (state/get) "x") (d/pair "x" "x"))))

  (t/testing "swap"
    (let [appendworld (state/swap (fn [st] (str st " world!")))]
      (t/is (= (state/exec appendworld "hello") "hello world!"))))

  (t/testing "wrap-fn"
    (t/is (= (state/run (state/wrap-fn (fn [] (+ 2 3))) 0) (d/pair 5 0))))

  (t/testing "post-increment"
    (t/is (= (state/run postincrement 1) (d/pair 1 2)))))
