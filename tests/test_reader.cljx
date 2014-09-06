(ns test-reader
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.protocols :as p]
            [cats.builtin :as b]
            [cats.monad.reader :as reader]
            [cats.monad.maybe :as maybe])
   #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-monad)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet with-monad]]
            [cats.protocols :as p]
            [cats.builtin :as b]
            [cats.monad.reader :as reader]
            [cats.monad.maybe :as maybe]))

(deftest test-reader-monad
  (testing "The `ask` reader gives you access to the environment"
    (is (= (reader/run-reader reader/ask {:foo "bar"})
           {:foo "bar"})))

  (testing "The `local` function allows you to run readers in a modified environment"
    (is (= (reader/run-reader (reader/local inc reader/ask) 41)
           42)))

  (testing "The monadic values can be mapped over"
    (is (= (reader/run-reader (m/fmap inc reader/ask) 41)
           42)))
 )

(deftest test-reader-transformerformer
  (let [maybe-reader (reader/reader-transformer maybe/maybe-monad)]
    (testing "The `ask` reader gives you access to the environment"
      (is (= (with-monad maybe-reader
               (reader/run-reader reader/ask {:foo "bar"}))
             (maybe/just {:foo "bar"}))))

    (testing "The `local` function allows you to run readers in a modified environment"
      (is (= (with-monad maybe-reader
               (reader/run-reader (reader/local inc reader/ask) 41))
             (maybe/just 42))))

    (testing "Monadic values can be lifted to the reader transformer"
      (is (= (with-monad maybe-reader
               (reader/run-reader (m/lift (maybe/just 42)) {}))
             (maybe/just 42))))

    (testing "The monad transformer values can be mapped over"
      (is (= (with-monad maybe-reader
               (reader/run-reader (m/fmap inc reader/ask) 41))
             (maybe/just 42)))))
)
