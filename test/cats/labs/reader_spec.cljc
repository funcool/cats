(ns cats.labs.reader-spec
  #?(:cljs
     (:require [cljs.test :as t]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.labs.reader :as reader]
               [cats.monad.maybe :as maybe]
               [cats.context :as ctx :include-macros true]
               [cats.core :as m :include-macros true])
     :clj
     (:require [clojure.test :as t]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.labs.reader :as reader]
               [cats.monad.maybe :as maybe]
               [cats.context :as ctx]
               [cats.core :as m])))

(t/deftest access-to-the-environment-test
  ;;The `ask` reader gives you access to the environment
  (t/is (= {:foo "bar"} (reader/run-reader reader/ask {:foo "bar"}))))

(t/deftest run-readers-in-a-modified-environment-test
  ;; The `local` function allows you to run readers in a modified environment
  (t/is (= 42 (reader/run-reader (reader/local inc reader/ask) 41))))

(t/deftest monadic-values-can-be-mapped-over-test
  ;; The monadic values can be mapped over
  (t/is (= 42 (reader/run-reader (m/fmap inc reader/ask) 41))))


(def maybe-reader-t (reader/reader-transformer maybe/context))

(t/deftest reader-transformerformer-tests
  ;; The `ask` reader gives you access to the environment
  (t/is (= (maybe/just {:foo "bar"})
           (ctx/with-context maybe-reader-t
             (reader/run-reader reader/ask {:foo "bar"}))))

  ;; The `local` function allows you to run readers in a modified environment
  (t/is (= (maybe/just 42)
           (ctx/with-context maybe-reader-t
             (reader/run-reader (reader/local inc reader/ask) 41))))

  ;; Monadic values can be lifted to the reader transformer
  (t/is (= (maybe/just 42)
           (ctx/with-context maybe-reader-t
             (reader/run-reader (m/lift (maybe/just 42)) {}))))

  ;; The monad transformer values can be mapped over
  (t/is (= (maybe/just 42)
           (ctx/with-context maybe-reader-t
             (reader/run-reader (m/fmap inc reader/ask) 41)))))
