(ns test-exception
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.protocols :as p]
            [cats.builtin :as b]
            [cats.monad.either :as either]
            [cats.monad.exception :as exc])
   #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-monad)]
                   [cats.monad.exception :refer (try-on try-or-else try-or-recover)])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet with-monad]]
            [cats.protocols :as p]
            [cats.builtin :as b]
            [cats.monad.either :as either]
            [cats.monad.exception :as exc :refer (try-on try-or-else try-or-recover)]))

(deftest test-exception-monad
  #+clj
  (testing "Basic operations clj."
    (let [e (Exception. "test")]
      (is (= 1 (exc/from-success (try-on 1))))
      (is (= e (exc/from-failure (try-on (throw e)))))
      (is (= e (exc/from-failure (try-on e))))))

  #+cljs
  (testing "Basic operations with cljs."
    (let [e (js/Error. "test")]
      (is (= 1 (exc/from-success (try-on 1))))
      (is (= e (exc/from-failure (try-on (throw e)))))
      (is (= e (exc/from-failure (try-on e))))))

  (testing "Test predicates"
    (let [m1 (exc/success 1)
          m2 (exc/failure 1)]
      (is (exc/success? m1))
      (is (exc/failure? m2))))

  #+clj
  (testing "Test wrap"
    (let [func (fn [x] (+ x nil))
          func (exc/wrap func)]
      (is (exc/failure? (func 3)))))

  #+cljs
  (testing "Test wrap"
    (let [func (fn [x] (throw (js/Error. "test")))
          func (exc/wrap func)]
      (is (exc/failure? (func 3)))))

  #+clj
  (testing "Test try-or-else macro"
    (let [m1 (try-or-else (+ 1 nil) 40)]
      (is (exc/success? m1))
      (is (= 40 (exc/from-try m1)))))

  #+clj
  (testing "Test try-or-recover macro clj"
    (let [m1 (try-or-recover (+ 1 nil) (fn [e] (m/return 60)))]
      (is (exc/success? m1))
      (is (= 60 (exc/from-try m1))))

    (let [m1 (try-or-recover
              (+ 1 nil)
              (fn [e] (either/right 60)))]
      (is (either/right? m1))
      (is (= 60 (either/from-either m1)))))

  #+cljs
  (testing "Test try-or-recover macro with cljs"
    (let [e  (js/Error. "test")
          m1 (try-or-recover e (fn [e] (m/return 60)))]
      (is (exc/success? m1))
      (is (= 60 (exc/from-try m1))))

    (let [e  (js/Error. "test")
          m1 (try-or-recover
              e
              (fn [e] (either/right 60)))]
      (is (either/right? m1))
      (is (= 60 (either/from-either m1)))))

  #+clj
  (testing "Test try-on macro"
    (let [m1 (try-on (+ 1 nil))]
      (is (instance? NullPointerException (exc/from-failure m1)))))

  #+cljs
  (testing "Test try-on macro"
    (let [m1 (try-on (js/Error. "foo"))]
      (is (instance? js/Error (exc/from-failure m1)))))

  #+clj
  (testing "Test fmap"
    (let [m1 (try-on 1)
          m2 (try-on nil)]
      (is (instance? NullPointerException
                     (exc/from-try (m/fmap inc m2))))
      (is (= (exc/success 2) (m/fmap inc m1)))))

  (testing "The first monad law: left identity"
    (is (= (exc/success 2)
           (m/>>= (p/mreturn exc/exception-monad 2)
                  exc/success)))

    (is (= (exc/success 2)
           (m/>>= (p/mreturn exc/exception-monad 2)
                  exc/success
                  exc/success))))

  (testing "The second monad law: right identity"
    (is (= (exc/success 2)
           (m/>>= (try-on 2) m/return))))

  (testing "The third monad law: associativity"
    (is (= (m/>>= (mlet [x (try-on 2)
                         y (try-on (inc x))]
                    (m/return y))
                  (fn [y] (try-on (inc y))))
           (m/>>= (try-on 2)
                  (fn [x]
                    (m/>>= (try-on (inc x))
                           (fn [y] (try-on (inc y)))))))))

)
