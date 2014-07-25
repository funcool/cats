(ns test-identity
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.protocols :as p]
            [cats.monad.identity :as id])
   #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-monad)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet with-monad]]
            [cats.protocols :as p]
            [cats.builtin :as b]
            [cats.monad.identity :as id]))

(deftest test-identity-monad
  (testing "Test fmap"
    (is (= (id/identity 2)
           (m/fmap inc (id/identity 1)))))

  (testing "The first monad law: left identity"
    (is (= (id/identity 2)
           (m/>>= (p/mreturn id/identity-monad 2) id/identity))))

  (testing "The second monad law: right identity"
    (is (= (id/identity 2)
           (m/>>= (id/identity 2) m/return))))

  (testing "The third monad law: associativity"
    (is (= (m/>>= (mlet [x (id/identity 2)
                         y (id/identity (inc x))]
                    (m/return y))
                  (fn [y] (id/identity (inc y))))
           (m/>>= (id/identity 2)
                  (fn [x] (m/>>= (id/identity (inc x))
                                 (fn [y] (id/identity (inc y)))))))))
)
