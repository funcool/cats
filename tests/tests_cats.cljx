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

; FIXME: cljs
#+clj
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

(deftest test-lift-m
  (testing "It can lift a function to the Maybe monad"
    (let [monad+ (m/lift-m +)]
      (is (= (t/just 6)
             (monad+ (t/just 1) (t/just 2) (t/just 3))))
      (is (= (t/nothing)
             (monad+ (t/just 1) (t/nothing)))))))

(deftest test-filter-m
  (testing "It can filter Maybe monadic values"
    (let [bigger-than-4 (partial < 4)]
      (is (= (t/just 6)
             (m/filter-m bigger-than-4 (t/just 6))))
      (is (= (t/nothing)
             (m/filter-m bigger-than-4 (t/just 3))))))
  (testing "It can filter vectors"
    (is (= [1 3 5]
           (m/filter-m odd? [1 2 3 4 5 6])))))

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

  (testing "sequence-m"
    (is (= (m/sequence-m [(t/just 2) (t/just 3)])
           (t/just [2 3])))
    (is (= (m/sequence-m [(t/just 2) (t/nothing)])
           (t/nothing)))))
