(ns cats.monad.reader-spec
  #+cljs
  (:require [cljs.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.reader :as reader]
            [cats.monad.maybe :as maybe]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [clojure.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.reader :as reader]
            [cats.monad.maybe :as maybe]
            [cats.core :as m]))

(t/deftest reader-monad-tests
  (t/testing "The `ask` reader gives you access to the environment"
    (t/is (= {:foo "bar"} (reader/run-reader reader/ask {:foo "bar"}))))

  (t/testing "The `local` function allows you to run readers in a modified environment"
    (t/is (= 42 (reader/run-reader (reader/local inc reader/ask) 41))))

  (t/testing "The monadic values can be mapped over"
    (t/is (= 42 (reader/run-reader (m/fmap inc reader/ask) 41)))))


(def maybe-reader (reader/reader-transformer maybe/maybe-monad))

(t/deftest reader-transformerformer-tests
  (t/testing "The `ask` reader gives you access to the environment"
    (t/is (= (maybe/just {:foo "bar"})
             (m/with-monad maybe-reader
               (reader/run-reader reader/ask {:foo "bar"})))))

  (t/testing "The `local` function allows you to run readers in a modified environment"
    (t/is (= (maybe/just 42)
             (m/with-monad maybe-reader
               (reader/run-reader (reader/local inc reader/ask) 41)))))

  (t/testing "Monadic values can be lifted to the reader transformer"
    (t/is (= (maybe/just 42)
             (m/with-monad maybe-reader
               (reader/run-reader (m/lift (maybe/just 42)) {})))))

  (t/testing "The monad transformer values can be mapped over"
    (t/is (= (maybe/just 42)
             (m/with-monad maybe-reader
               (reader/run-reader (m/fmap inc reader/ask) 41))))))
