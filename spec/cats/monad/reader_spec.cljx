(ns cats.monad.reader-spec
  #+cljs
  (:require [speclj.core :as s :include-macros true]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.reader :as reader]
            [cats.monad.maybe :as maybe]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [speclj.core :as s]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.reader :as reader]
            [cats.monad.maybe :as maybe]
            [cats.core :as m]))

(s/describe "reader-monad"
  (s/it "The `ask` reader gives you access to the environment"
    (s/should= {:foo "bar"} (reader/run-reader reader/ask {:foo "bar"})))

  (s/it "The `local` function allows you to run readers in a modified environment"
    (s/should= 42 (reader/run-reader (reader/local inc reader/ask) 41)))

  (s/it "The monadic values can be mapped over"
    (s/should= 42 (reader/run-reader (m/fmap inc reader/ask) 41))))


(def maybe-reader (reader/reader-transformer maybe/maybe-monad))

(s/describe "reader-transformerformer"
  (s/it "The `ask` reader gives you access to the environment"
    (s/should= (maybe/just {:foo "bar"})
               (m/with-monad maybe-reader
                 (reader/run-reader reader/ask {:foo "bar"}))))

  (s/it "The `local` function allows you to run readers in a modified environment"
    (s/should= (maybe/just 42)
               (m/with-monad maybe-reader
                 (reader/run-reader (reader/local inc reader/ask) 41))))

  (s/it "Monadic values can be lifted to the reader transformer"
    (s/should= (maybe/just 42)
               (m/with-monad maybe-reader
                 (reader/run-reader (m/lift (maybe/just 42)) {}))))

  (s/it "The monad transformer values can be mapped over"
    (s/should= (maybe/just 42)
               (m/with-monad maybe-reader
                 (reader/run-reader (m/fmap inc reader/ask) 41)))))
