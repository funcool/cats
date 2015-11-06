(ns cats.monad.exception-spec
  #?@(:cljs
      [(:require [cljs.test :as t]
                 [clojure.test.check]
                 [clojure.test.check.generators :as gen]
                 [clojure.test.check.properties :as prop :include-macros true]
                 [cats.labs.test :as lt]
                 [cats.builtin :as b]
                 [cats.protocols :as p]
                 [cats.monad.exception :as exc :include-macros true]
                 [cats.monad.either :as either]
                 [cats.context :as ctx :include-macros true]
                 [cats.core :as m :include-macros true])
       (:require-macros [clojure.test.check.clojure-test :refer (defspec)])])
  #?(:clj
     (:require [clojure.test :as t]
               [clojure.test.check.clojure-test :refer [defspec]]
               [clojure.test.check :as tc]
               [clojure.test.check.generators :as gen]
               [clojure.test.check.properties :as prop]
               [cats.labs.test :as lt]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.monad.exception :as exc]
               [cats.monad.either :as either]
               [cats.context :as ctx]
               [cats.core :as m])))

(t/deftest basic-operations-test
  (let [e #?(:clj (Exception. "test")
             :cljs (js/Error. "test"))]
    (t/is (= 1 (m/extract (exc/try-on 1))))
    (t/is (= e (m/extract (exc/try-on (throw e)))))
    (t/is (= e (m/extract (exc/try-on e))))))

#?(:clj
   (t/deftest ideref-test
     (t/is (= 1 @(exc/success 1)))
     (t/is (thrown? Exception @(exc/failure {:message "foobar"})))))

(t/deftest predicates-test
  (let [m1 (exc/success 1)
        m2 (exc/failure {})]
    (t/is (exc/success? m1))
    (t/is (exc/failure? m2))))


(t/deftest wrapping-test
  (let [func #?(:clj (fn [x] (+ x nil))
                :cljs (fn [x] (throw (js/Error. "test"))))
        func (exc/wrap func)]
    (t/is (exc/failure? (func 3)))))

#?(:clj
   (t/deftest try-or-else-test
     (let [m1 (exc/try-or-else (+ 1 nil) 40)]
       (t/is (exc/success? m1))
       (t/is (= 40 (exc/extract m1))))))

#?(:clj
   (t/deftest try-or-recover-test
     (let [m1 (exc/try-or-recover (+ 1 nil) (fn [e] (m/return 60)))]
       (t/is (exc/success? m1))
       (t/is (= 60 (exc/extract m1))))

     (let [m1 (exc/try-or-recover
               (+ 1 nil)
               (fn [e] (either/right 60)))]
       (t/is (either/right? m1))
       (t/is (= 60 (m/extract m1))))))

#?(:cljs
   (t/deftest try-or-recover-test
     (let [e  (js/Error. "test")
           m1 (exc/try-or-recover e (fn [e] (m/return 60)))]
       (t/is (exc/success? m1))
       (t/is (= 60 (exc/extract m1))))

     (let [e  (js/Error. "test")
           m1 (exc/try-or-recover e (fn [e] (either/right 60)))]
       (t/is (either/right? m1))
       (t/is (= 60 (m/extract m1))))))

#?(:clj
   (t/deftest try-on-macro-test
     (let [m1 (exc/try-on (+ 1 nil))]
       (t/is (instance? NullPointerException (m/extract m1))))))

#?(:cljs
   (t/deftest try-on-macro-test
     (let [m1 (exc/try-on (js/Error. "foo"))]
       (t/is (instance? js/Error (m/extract m1))))))

#?(:clj
   (t/deftest functor-test
     (let [m1 (exc/try-on 1)
           m2 (exc/try-on nil)]
       (t/is (instance? NullPointerException
                        (m/extract (m/fmap inc m2))))
       (t/is (= (exc/success 2) (m/fmap inc m1))))))

;; Generators

(defn successes-of
  [g]
  (gen/fmap exc/success g))

(def success-gen
  (successes-of gen/any))

(def failure-gen
  (gen/return (exc/failure {})))

(def exc-gen
  (gen/one-of [success-gen failure-gen]))

(def vectors-gen
  (gen/vector gen/any))

;; Functor

(defspec exc-first-functor-law 10
  (lt/first-functor-law {:gen exc-gen}))

(defspec exc-second-functor-law 10
  (lt/second-functor-law {:gen exc-gen
                          :f str
                          :g count}))

;; Applicative

(defspec exc-applicative-identity 10
  (lt/applicative-identity-law {:ctx exc/context
                                :gen exc-gen}))

(defspec exc-applicative-homomorphism 10
  (lt/applicative-homomorphism {:ctx exc/context
                                :gen gen/any
                                :f (constantly false)}))

(defspec exc-applicative-interchange 10
  (lt/applicative-interchange {:ctx exc/context
                               :gen gen/int
                               :appf (exc/success inc)}))

(defspec exc-applicative-composition 10
  (lt/applicative-composition {:ctx exc/context
                               :gen gen/int
                               :appf (exc/success inc)
                               :appg (exc/success dec)}))

;; Monad

(defspec exc-first-monad-law 10
  (lt/first-monad-law {:ctx exc/context
                       :mf #(if % (exc/success %) (exc/failure {}))}))

(defspec exc-second-monad-law 10
  (lt/second-monad-law {:ctx exc/context}))

(defspec exc-third-monad-law 10
  (lt/third-monad-law {:ctx exc/context
                       :f (comp exc/success str)
                       :g (comp exc/success count)}))
