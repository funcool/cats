(ns cats.monad.exception-spec
  #+cljs
  (:require [cljs.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.exception :as exc :include-macros true]
            [cats.monad.either :as either]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [clojure.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.exception :as exc]
            [cats.monad.either :as either]
            [cats.core :as m]))

(t/deftest exception-monad-tests
  #+clj
  (t/testing "Basic operations clj."
    (let [e (Exception. "test")]
      (t/is (= 1 (exc/from-success (exc/try-on 1))))
      (t/is (= e (exc/from-failure (exc/try-on (throw e)))))
      (t/is (= e (exc/from-failure (exc/try-on e))))))

  (t/testing "Test IDeref"
    (t/is (= 1 @(exc/success 1)))
    (t/is (= 1 @(exc/failure 1))))

  #+cljs
  (t/testing "Basic operations with cljs."
    (let [e (js/Error. "test")]
      (t/is (= 1 (exc/from-success (exc/try-on 1))))
      (t/is (= e (exc/from-failure (exc/try-on (throw e)))))
      (t/is (= e (exc/from-failure (exc/try-on e))))))

  (t/testing "Test predicates"
    (let [m1 (exc/success 1)
          m2 (exc/failure 1)]
      (t/is (exc/success? m1))
      (t/is (exc/failure? m2))))

  #+clj
  (t/testing "Test wrap"
    (let [func (fn [x] (+ x nil))
          func (exc/wrap func)]
      (t/is (exc/failure? (func 3)))))

  #+cljs
  (t/testing "Test wrap"
    (let [func (fn [x] (throw (js/Error. "test")))
          func (exc/wrap func)]
      (t/is (exc/failure? (func 3)))))

  #+clj
  (t/testing "Test try-or-else macro"
    (let [m1 (exc/try-or-else (+ 1 nil) 40)]
      (t/is (exc/success? m1))
      (t/is (= 40 (exc/from-try m1)))))

  #+clj
  (t/testing "Test try-or-recover macro clj"
    (let [m1 (exc/try-or-recover (+ 1 nil) (fn [e] (m/return 60)))]
      (t/is (exc/success? m1))
      (t/is (= 60 (exc/from-try m1))))

    (let [m1 (exc/try-or-recover
              (+ 1 nil)
              (fn [e] (either/right 60)))]
      (t/is (either/right? m1))
      (t/is (= 60 (either/from-either m1)))))

  #+cljs
  (t/testing "Test try-or-recover macro with cljs"
    (let [e  (js/Error. "test")
          m1 (exc/try-or-recover e (fn [e] (m/return 60)))]
      (t/is (exc/success? m1))
      (t/is (= 60 (exc/from-try m1))))

    (let [e  (js/Error. "test")
          m1 (exc/try-or-recover
              e
              (fn [e] (either/right 60)))]
      (t/is (either/right? m1))
      (t/is (= 60 (either/from-either m1)))))

  #+clj
  (t/testing "Test try-on macro"
    (let [m1 (exc/try-on (+ 1 nil))]
      (t/is (instance? NullPointerException (exc/from-failure m1)))))

  #+cljs
  (t/testing "Test try-on macro"
    (let [m1 (exc/try-on (js/Error. "foo"))]
      (t/is (instance? js/Error (exc/from-failure m1)))))

  #+clj
  (t/testing "Test fmap"
    (let [m1 (exc/try-on 1)
          m2 (exc/try-on nil)]
      (t/is (instance? NullPointerException
                       (exc/from-try (m/fmap inc m2))))
      (t/is (= (exc/success 2) (m/fmap inc m1)))))

  (t/testing "The first monad law: left identity"
    (t/is (= (exc/success 2)
             (m/>>= (p/mreturn exc/exception-monad 2)
                    exc/success)))

    (t/is (= (exc/success 2)
             (m/>>= (p/mreturn exc/exception-monad 2)
                    exc/success
                    exc/success))))

  (t/testing "The second monad law: right identity"
    (t/is (= (exc/success 2)
             (m/>>= (exc/try-on 2) m/return))))

  (t/testing "The third monad law: associativity"
    (t/is (= (m/>>= (m/mlet [x (exc/try-on 2)
                             y (exc/try-on (inc x))]
                      (m/return y))
                    (fn [y] (exc/try-on (inc y))))
             (m/>>= (exc/try-on 2)
                    (fn [x]
                      (m/>>= (exc/try-on (inc x))
                             (fn [y] (exc/try-on (inc y))))))))))
