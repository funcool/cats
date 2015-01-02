(ns cats.monad.writer-spec
  #+cljs
  (:require [speclj.core :as s :include-macros true]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.data :as d]
            [cats.monad.writer :as writer]
            [cats.monad.maybe :as maybe]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [speclj.core :as s]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.data :as d]
            [cats.monad.writer :as writer]
            [cats.monad.maybe :as maybe]
            [cats.core :as m]))


(s/describe "writer-monad"
  (s/it "Putting a value in a writer context yields an empty log"
    (s/should= 42
               (writer/value (p/mreturn writer/writer-monad 42)))
    (s/should= []
               (writer/log (p/mreturn writer/writer-monad 42))))

  (s/it "The `tell` function adds the given value to the log"
    (s/should= ["Hello" "world"]
               (m/with-monad writer/writer-monad
                 (writer/log (m/>> (writer/tell "Hello")
                                   (writer/tell "world"))))))

  (s/it "The `listen` function yields a pair with the value and the log"
    (let [w (m/with-monad writer/writer-monad
              (m/>> (writer/tell "Hello")
                    (writer/tell "world")
                    (m/return 42)))
          w (writer/listen w)]
      (s/should= (d/pair 42 ["Hello" "world"])
                 (first w))
      (s/should= ["Hello" "world"]
                 (second w))))

  (s/it "The `pass` function can be used to apply a function to the log"
    (let [w (m/with-monad writer/writer-monad
              (m/>> (writer/tell "Hello")
                    (writer/tell "world")
                    (m/return [42 reverse])))
          w (writer/listen (writer/pass w))]
        (s/should= (d/pair 42 ["world" "Hello"])
                   (first w))
        (s/should= ["world" "Hello"]
                   (second w)))))


(def maybe-writer (writer/writer-transformer maybe/maybe-monad))

(s/describe "writer-transformerformer"
  (s/it "Putting a value in a writer transformer context yields an empty log"
    (let [w (m/with-monad maybe-writer
              (m/return 42))]
      (s/should= 42
                 (writer/value (maybe/from-maybe w)))
      (s/should= []
                 (writer/log (maybe/from-maybe w)))))

  (s/it "The `tell` function adds the given value to the log"
    (let [w (m/with-monad maybe-writer
              (m/>> (writer/tell "Hello")
                    (writer/tell "world")))]
      (s/should= ["Hello" "world"]
                 (writer/log (maybe/from-maybe w)))))

  (s/it "The `listen` function yields a pair with the value and the log"
    (let [w (m/with-monad maybe-writer
              (m/>> (writer/tell "Hello")
                    (writer/tell "world")
                    (m/return 42)))
          w (m/with-monad maybe-writer
              (writer/listen w))]
      (s/should= (d/pair 42 ["Hello" "world"])
                 (first (maybe/from-maybe w)))
      (s/should= ["Hello" "world"]
                 (second (maybe/from-maybe w)))))

  (s/it "The `pass` function can be used to apply a function to the log"
    (let [w (m/with-monad maybe-writer
              (m/>> (writer/tell "Hello")
                    (writer/tell "world")
                    (m/return [42 reverse])))
          w (m/with-monad maybe-writer
              (writer/listen (writer/pass w)))]
      (s/should= (d/pair 42 ["world" "Hello"])
                 (first (maybe/from-maybe w)))
      (s/should= ["world" "Hello"]
                 (second (maybe/from-maybe w)))))

  (s/it "Inner monad values can be lifted into the transformer"
    (let [lifted-just (m/with-monad maybe-writer
                        (m/lift (maybe/just 3)))
          lifted-nothing (m/with-monad maybe-writer
                           (m/lift (maybe/nothing)))]
      (s/should= (maybe/just (d/pair 3 ["Hello"]))
                 (m/with-monad maybe-writer
                   (m/>> (writer/tell "Hello")
                         lifted-just)))
      (s/should= (maybe/nothing)
                 (m/with-monad maybe-writer
                   (m/>> (writer/tell "Hello")
                         lifted-nothing))))))
