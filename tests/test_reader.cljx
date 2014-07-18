(ns test-reader
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.protocols :as p]
            [cats.builtin :as b]
            [cats.monad.reader :as reader])
   #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-monad)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet with-monad]]
            [cats.protocols :as p]
            [cats.builtin :as b]
            [cats.monad.reader :as reader]))

(deftest test-reader-monad
  (testing "The `ask` reader gives you access to the environment"
    (is (= (reader/run-reader reader/ask {:foo "bar"})
           {:foo "bar"})))

  (testing "The `local` function allows you to run readers in a modified environment"
    (is (= (reader/run-reader (reader/local inc reader/ask) 41)
           42))))
