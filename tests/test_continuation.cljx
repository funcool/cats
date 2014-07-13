(ns test-continuation
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.monad.continuation :as cont])
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-context)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet with-context]]
            [cats.monad.continuation :as cont]))


(deftest test-continuation-monad
  (let [cont-42 (cont/continuation (fn [c] (c 42)))
        inc-cont-fn (fn [x]
                      (cont/continuation (fn [c] (c (inc x)))))]

    (testing "The first monad law: left identity"
      (is (= (cont/run-cont cont-42)
             (cont/run-cont
               (with-context cont/continuation-monad
                 (m/>>= (m/return 42)
                        (fn [v] (cont/continuation (fn [c] c v)))))))))

    (testing "The second monad law: right identity"
      (is (= (cont/run-cont cont-42)
             (cont/run-cont
               (m/>>= cont-42 m/return)))))

    (testing "The third monad law: associativity"
      (is (= (m/>>= (mlet [x  cont-42
                           y  inc-cont-fn]
                         (m/return y))
                         inc-cont-fn))
             (m/>>= cont-42
                    (fn [x] (m/>>= (cont/continuation (fn [c] (c (inc x))))
                                   inc-cont-fn)))))

    (testing "call-cc allows the creation of resumable computations."
      (let [cc (atom nil)]
        (is (= 44
               (cont/run-cont (mlet [x cont-42
                                  y (cont/call-cc (fn [k]
                                                    (reset! cc k)
                                                    (k 2)))]
                                 (m/return (+ x y))))))
        (is (= 45
               (cont/run-cont (@cc 3))))
        (is (= 46
               (cont/run-cont (@cc 4))))))))
