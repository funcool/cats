(ns cats.monad.exception-spec
  #+cljs
  (:require [speclj.core :as s :include-macros true]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.exception :as exc :include-macros true]
            [cats.monad.either :as either]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [speclj.core :as s]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.exception :as exc]
            [cats.monad.either :as either]
            [cats.core :as m]))

(s/describe "exception-monad"
  #+clj
  (s/it "Basic operations clj."
    (let [e (Exception. "test")]
      (s/should= 1 (exc/from-success (exc/try-on 1)))
      (s/should= e (exc/from-failure (exc/try-on (throw e))))
      (s/should= e (exc/from-failure (exc/try-on e)))))

  (s/it "Test IDeref"
    (s/should= 1 @(exc/success 1))
    (s/should= 1 @(exc/failure 1)))

  #+cljs
  (s/it "Basic operations with cljs."
    (let [e (js/Error. "test")]
      (s/should= 1 (exc/from-success (exc/try-on 1)))
      (s/should= e (exc/from-failure (exc/try-on (throw e))))
      (s/should= e (exc/from-failure (exc/try-on e)))))

  (s/it "Test predicates"
    (let [m1 (exc/success 1)
          m2 (exc/failure 1)]
      (s/should (exc/success? m1))
      (s/should (exc/failure? m2))))

  #+clj
  (s/it "Test wrap"
    (let [func (fn [x] (+ x nil))
          func (exc/wrap func)]
      (s/should (exc/failure? (func 3)))))

  #+cljs
  (s/it "Test wrap"
    (let [func (fn [x] (throw (js/Error. "test")))
          func (exc/wrap func)]
      (s/should (exc/failure? (func 3)))))

  #+clj
  (s/it "Test try-or-else macro"
    (let [m1 (exc/try-or-else (+ 1 nil) 40)]
      (s/should (exc/success? m1))
      (s/should= 40 (exc/from-try m1))))

  #+clj
  (s/it "Test try-or-recover macro clj"
    (let [m1 (exc/try-or-recover (+ 1 nil) (fn [e] (m/return 60)))]
      (s/should (exc/success? m1))
      (s/should= 60 (exc/from-try m1)))

    (let [m1 (exc/try-or-recover
              (+ 1 nil)
              (fn [e] (either/right 60)))]
      (s/should (either/right? m1))
      (s/should= 60 (either/from-either m1))))

  #+cljs
  (s/it "Test try-or-recover macro with cljs"
    (let [e  (js/Error. "test")
          m1 (exc/try-or-recover e (fn [e] (m/return 60)))]
      (s/should (exc/success? m1))
      (s/should= 60 (exc/from-try m1)))

    (let [e  (js/Error. "test")
          m1 (exc/try-or-recover
              e
              (fn [e] (either/right 60)))]
      (s/should (either/right? m1))
      (s/should= 60 (either/from-either m1))))

  #+clj
  (s/it "Test try-on macro"
    (let [m1 (exc/try-on (+ 1 nil))]
      (s/should (instance? NullPointerException (exc/from-failure m1)))))

  #+cljs
  (s/it "Test try-on macro"
    (let [m1 (exc/try-on (js/Error. "foo"))]
      (s/should (instance? js/Error (exc/from-failure m1)))))

  #+clj
  (s/it "Test fmap"
    (let [m1 (exc/try-on 1)
          m2 (exc/try-on nil)]
      (s/should (instance? NullPointerException
                           (exc/from-try (m/fmap inc m2))))
      (s/should= (exc/success 2) (m/fmap inc m1))))

  (s/it "The first monad law: left identity"
    (s/should= (exc/success 2)
               (m/>>= (p/mreturn exc/exception-monad 2)
                      exc/success))

    (s/should= (exc/success 2)
               (m/>>= (p/mreturn exc/exception-monad 2)
                      exc/success
                      exc/success)))

  (s/it "The second monad law: right identity"
    (s/should= (exc/success 2)
           (m/>>= (exc/try-on 2) m/return)))

  (s/it "The third monad law: associativity"
    (s/should= (m/>>= (m/mlet [x (exc/try-on 2)
                               y (exc/try-on (inc x))]
                        (m/return y))
                      (fn [y] (exc/try-on (inc y))))
               (m/>>= (exc/try-on 2)
                      (fn [x]
                        (m/>>= (exc/try-on (inc x))
                               (fn [y] (exc/try-on (inc y)))))))))
