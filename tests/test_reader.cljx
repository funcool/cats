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
    (let [env {:x 42 :y nil}]
      (m/mlet [renv reader/ask]
        (is (= renv env))
        (m/return nil)))))
