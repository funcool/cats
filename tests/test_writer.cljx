(ns test-writer
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.protocols :as p]
            [cats.data :as d]
            [cats.monad.maybe :as maybe]
            [cats.monad.writer :as writer])
   #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-monad)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet with-monad]]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.data :as d]
            [cats.monad.maybe :as maybe]
            [cats.monad.writer :as writer]))

(deftest test-writer-monad
  (testing "Putting a value in a writer context yields an empty log"
    (is (= 42
           (writer/value (p/mreturn writer/writer-monad 42))))
    (is (= []
           (writer/log (p/mreturn writer/writer-monad 42)))))

  (testing "The `tell` function adds the given value to the log"
    (is (= ["Hello" "world"]
           (with-monad writer/writer-monad
             (writer/log (m/>> (writer/tell "Hello")
                               (writer/tell "world")))))))

  (testing "The `listen` function yields a pair with the value and the log"
    (let [w (with-monad writer/writer-monad
              (m/>> (writer/tell "Hello")
                    (writer/tell "world")
                    (m/return 42)))
          w (writer/listen w)]
        (is (= (d/pair 42 ["Hello" "world"])
               (first w)))
        (is (= ["Hello" "world"]
               (second w)))))

  (testing "The `pass` function can be used to apply a function to the log"
    (let [w (with-monad writer/writer-monad
              (m/>> (writer/tell "Hello")
                    (writer/tell "world")
                    (m/return [42 reverse])))
          w (writer/listen (writer/pass w))]
        (is (= (d/pair 42 ["world" "Hello"])
               (first w)))
        (is (= ["world" "Hello"]
               (second w)))))
)

(deftest test-writer-transformer
  (let [maybe-writer (writer/writer-trans maybe/maybe-monad)]
    (testing "Putting a value in a writer transformer context yields an empty log"
      (let [w (with-monad maybe-writer
                (m/return 42))]
        (is (= 42
               (writer/value (maybe/from-maybe w))))
        (is (= []
               (writer/log (maybe/from-maybe w))))))

    (testing "The `tell` function adds the given value to the log"
      (let [w (with-monad maybe-writer
                 (m/>> (writer/tell "Hello")
                       (writer/tell "world")))]
        (is (= ["Hello" "world"]
               (writer/log (maybe/from-maybe w))))))


    (testing "The `listen` function yields a pair with the value and the log"
      (let [w (with-monad maybe-writer
                (m/>> (writer/tell "Hello")
                      (writer/tell "world")
                      (m/return 42)))
            w (with-monad maybe-writer
                (writer/listen w))]
          (is (= (d/pair 42 ["Hello" "world"])
                 (first (maybe/from-maybe w))))
          (is (= ["Hello" "world"]
                 (second (maybe/from-maybe w))))))

    (testing "The `pass` function can be used to apply a function to the log"
      (let [w (with-monad maybe-writer
                (m/>> (writer/tell "Hello")
                      (writer/tell "world")
                      (m/return [42 reverse])))
            w (with-monad maybe-writer
                (writer/listen (writer/pass w)))]
          (is (= (d/pair 42 ["world" "Hello"])
                 (first (maybe/from-maybe w))))
          (is (= ["world" "Hello"]
                 (second (maybe/from-maybe w)))))))
)
