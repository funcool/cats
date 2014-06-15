(ns tests-cats
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)])
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.types :as t])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m]
            [cats.types :as t]))


(deftest test-mlet
  (testing "It supports regular let inside its bindings")
    (is (= (t/just 2)
           (m/mlet [i (t/just 1)
                    :let [i (inc i)]]
                   (m/return i))))
  (testing "It supports :when guards inside its bindings")
    (is (= (t/nothing)
           (m/mlet [i (t/just 2)
                    :when (> i 2)]
                   (m/return i))))
    (is (= [3 4 5]
           (m/mlet [i [1 2 3 4 5]
                    :when (> i 2)]
                   (m/return i)))))

(deftest test-maybe
  (testing "Test predicates"
    (let [m1 (t/just 1)]
      (is (t/maybe? m1))
      (is (t/just? m1))))

  (testing "Test fmap"
    (let [m1 (t/just 1)
          m2 (t/nothing)]
      (is (= (m/fmap inc m1) (t/just 2)))
      (is (= (m/fmap inc m2) (t/nothing)))))

  #+clj
  (testing "sequence-m"
    (is (= (m/sequence-m [(t/just 2) (t/just 3)])
           (t/just [2 3])))
    (is (= (m/sequence-m [(t/just 2) (t/nothing)])
           (t/nothing)))))
