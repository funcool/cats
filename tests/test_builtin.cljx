(ns test-builtin
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.builtin :as b]
            [cats.core :as m])
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-context)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.builtin :as b]
            [cats.core :as m :refer [mlet with-context]]))


(deftest test-vector-monad
  (testing "The first monad law: left identity"
    (is (= [1 2 3 4 5]
           (m/>>= [0 1 2 3 4]
                  (fn [x] [(inc x)])))))
  (testing "The second law: right identity"
    (is (= [1 2 3]
           (m/>>= [1 2 3]
                  m/return))))
  (testing "The third law: associativity"
    (is (= (m/>>= (mlet [x [1 2 3 4 5]
                         y [(inc x)]]
                        (m/return y))
                  (fn [z] [(inc z)]))

           (m/>>= [1 2 3 4 5]
                  (fn [x] (m/>>= [(inc x)]
                                (fn [y] [(inc y)]))))))))

(deftest test-sequence-monad
  (let [val->lazyseq (fn [x] (lazy-seq [x]))
        s (val->lazyseq 2)]
    (testing "The first monad law: left identity"
      (is (= s
             (with-context b/sequence-monad
               (m/>>= (m/return 2)
                      val->lazyseq)))))

    (testing "The second monad law: right identity"
      (is (= s
             (m/>>= s
                    m/return))))

    (testing "The third monad law: associativity"
      (is (= (m/>>= (mlet [x  s
                           y  (val->lazyseq (inc x))]
                          (m/return y))
                    (fn [y] (val->lazyseq (inc y))))
             (m/>>= s
                    (fn [x] (m/>>= (val->lazyseq (inc x))
                                  (fn [y] (val->lazyseq (inc y)))))))))))
