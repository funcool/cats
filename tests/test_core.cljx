(ns test-core
  #+cljs
  (:require [cats.core :as m]
            [cats.monad.maybe :as maybe])
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet lift-m)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet lift-m]]
            [cats.builtin :refer :all]
            [cats.monad.maybe :as maybe])
  (:use [cats.builtin :only [sequence-monad]]))


(deftest test-mlet
  (testing "It supports regular let inside its bindings")
    (is (= (maybe/just 2)
           (mlet [i (maybe/just 1)
                  :let [i (inc i)]]
                 (m/return i))))
  (testing "It supports :when guards inside its bindings")
    (is (= (maybe/nothing)
           (mlet [i (maybe/just 2)
                  :when (> i 2)]
                 (m/return i))))
    (is (= [3 4 5]
           (mlet [i [1 2 3 4 5]
                  :when (> i 2)]
                 (m/return i))))
  (testing "The body runs in an implicit do"
    (is (= (maybe/just 3)
           (mlet [i (maybe/just 2)
                  :let [x (inc i)]]
                 (assert (= x 3))
                 (m/return x))))))

(deftest test-sequence
  (testing "It works with vectors"
    (is (= (m/sequence [[1 2] [3 4]])
           [[1 3] [1 4] [2 3] [2 4]])))
  (testing "It works with lazy seqs"
    (is (= (m/sequence [(lazy-seq [1 2]) (lazy-seq [3 4])])
           [[1 3] [1 4] [2 3] [2 4]])))
  (testing "It works with sets"
    (is (= (m/sequence [#{1 2} #{3 4}])
           #{[1 3] [1 4] [2 3] [2 4]})))
  (testing "It works with Maybe values"
    (is (= (m/sequence [(maybe/just 2) (maybe/just 3)])
           (maybe/just [2 3])))
    (is (= (m/sequence [(maybe/just 2) (maybe/nothing)])
           (maybe/nothing)))))

(deftest test-mapseq
  (testing "It works with maybe values"
    (is (= (m/mapseq maybe/just [1 2 3 4 5])
           (maybe/just [1 2 3 4 5])))
    (is (= (maybe/nothing)
           (m/mapseq (fn [v]
                        (if (odd? v)
                          (maybe/just v)
                          (maybe/nothing)))
                      [1 2 3 4 5])))))

(deftest test-lift-m
  (let [monad+ (lift-m 2 +)]
    (testing "It can lift a function to the vector monad"
      (is (= [1 2 3 4 5 6]
             (monad+ [0 2 4] [1 2]))))
    (testing "It can lift a function to the Maybe monad"
      (is (= (maybe/just 6)
             (monad+ (maybe/just 2) (maybe/just 4))))
      (is (= (maybe/nothing)
             (monad+ (maybe/just 1) (maybe/nothing)))))
    #+clj
    (testing "It can lift a function to a Monad Transformer"
      (let [maybe-sequence-monad (maybe/maybe-transformer sequence-monad)]
        (is (= [(maybe/just 1) (maybe/just 2) (maybe/just  3) (maybe/just  4) (maybe/just  5) (maybe/just 6)]
               (m/with-monad maybe-sequence-monad
                 (monad+ [(maybe/just 0) (maybe/just 2) (maybe/just 4)]
                         [(maybe/just 1) (maybe/just 2)]))))))))

(deftest test-filter
  (testing "It can filter Maybe monadic values"
    (let [bigger-than-4 (partial < 4)]
      (is (= (maybe/just 6)
             (m/filter bigger-than-4 (maybe/just 6))))
      (is (= (maybe/nothing)
             (m/filter bigger-than-4 (maybe/just 3))))))
  (testing "It can filter vectors"
    (is (= [1 3 5]
           (m/filter odd? [1 2 3 4 5 6])))))

(deftest test-when
  (testing "It returns the monadic value unchanged when the condition is true"
    (is (= (maybe/just 3)
           (m/when true (maybe/just 3)))))
  (testing "It returns nil in the monadic context when the condition is false"
    (is (= [nil]
           (m/when false [])))))
