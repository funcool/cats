(ns test-writer
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.protocols :as p]
            [cats.data :as d]
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
            [cats.monad.writer :as writer]))

; FIXME: functions for extracting either log or value more state-monad like

(deftest test-writer-monad
  (testing "Putting a value in a writer context yields an empty log"
    (is (= 42
           (writer/value (p/mreturn writer/writer-monad 42)))))

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

  (testing "The `listen` function yields a pair with the value and the log"
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
