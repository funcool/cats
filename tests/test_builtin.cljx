(ns test-builtin
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m])
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet]]))


(deftest test-vector
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

;(deftest test-lazy-seq
;  (let [s (lazy-seq [2])
;        val->lazyseq (fn [x] (lazy-seq [x]))]
;    (testing "The first monad law: left identity"
;      (is (= s
;             (with-context s
;               (m/>>= (m/return 2)
;                      val->lazyseq)))))
;
;    (testing "The second monad law: right identity"
;      (is (= s
;             (m/>>= s
;                    m/return))))
;
;    (testing "The third monad law: associativity"
;      (is (= (m/>>= (mlet [x  s
;                           y  (val->lazyseq (inc x))]
;                          (m/return y))
;                    (fn [y] (val->lazyseq (inc y))))
;             (m/>>= s
;                    (fn [x] (m/>>= (val->lazyseq (inc x))
;                                  (fn [y] (val->lazyseq (inc y)))))))))))
