(ns cats.applicative.validation-spec
  #?@(:cljs
      [(:require [cljs.test :as t]
                 [clojure.test.check]
                 [clojure.test.check.generators :as gen]
                 [clojure.test.check.properties :as prop :include-macros true]
                 [cats.labs.test :as lt]
                 [cats.builtin :as b]
                 [cats.protocols :as p]
                 [cats.applicative.validation :as validation]
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
               [cats.applicative.validation :as validation]
               [cats.monad.either :as either]
               [cats.context :as ctx]
               [cats.core :as m])))

(t/deftest basic-operations-test
  (t/is (= 42 (m/extract (validation/ok 42))))
  (t/is (= {:foo "bar"} (m/extract (validation/fail {:foo "bar"})))))

(t/deftest ideref-test
  (t/is (= 1 @(validation/fail 1)))
  (t/is (= 1 @(validation/ok 1))))

(t/deftest predicates-test
  (let [m1 (validation/ok 1)
        m2 (validation/fail 42)]
    (t/is (validation/validation? m1))
    (t/is (validation/validation? m2))
    (t/is (validation/ok? m1))
    (t/is (validation/fail? m2))))

(t/deftest either-conversion-test
  (let [ok1 (validation/ok 42)
        fail1 (validation/fail 42)
        left1 (either/left 42)
        right1 (either/right 42)]
    (t/is (= (either/right 42) (validation/validation->either ok1)))
    (t/is (= (either/left 42) (validation/validation->either fail1)))

    (t/is (= (validation/ok 42) (validation/either->validation right1)))
    (t/is (= (validation/fail 42) (validation/either->validation left1)))))

(t/deftest foldable-test
  (t/testing "Foldl"
    (t/is (= (validation/ok 2)
             (m/foldl #(m/pure (+ %1 %2)) 1 (validation/ok 1))))
    (t/is (= 1
             (m/foldl #(m/pure (+ %1 %2)) 1 (validation/fail)))))

  (t/testing "Foldr"
    (t/is (= (validation/ok 2)
             (m/foldr #(m/pure (+ %1 %2)) 1 (validation/ok 1))))
    (t/is (= 1
             (m/foldr #(m/pure (+ %1 %2)) 1 (validation/fail))))))

;; Generators

(defn oks-of
  [g]
  (gen/fmap validation/ok g))

(def ok-gen
  (oks-of gen/any))

(def fail-gen
  (gen/return (validation/fail [:oh-no])))

(def validation-gen
  (gen/one-of [ok-gen fail-gen]))

(def vectors-gen
  (gen/vector gen/any))

;; Semigroup

(defspec validation-semigroup 10
  (lt/semigroup-associativity {:ctx validation/context
                               :gen (oks-of (gen/not-empty vectors-gen))}))

;; Monoid

(defspec validation-monoid 10
  (lt/monoid-identity-element {:ctx validation/context
                               :gen (oks-of (gen/not-empty vectors-gen))
                               :empty (validation/fail [])}))

;; Functor

(defspec validation-first-functor-law 10
  (lt/first-functor-law {:gen validation-gen}))

(defspec validation-second-functor-law 10
  (lt/second-functor-law {:gen validation-gen
                          :f str
                          :g count}))

;; Applicative

(defspec validation-applicative-identity 10
  (lt/applicative-identity-law {:ctx validation/context
                                :gen validation-gen}))

(defspec validation-applicative-homomorphism 10
  (lt/applicative-homomorphism {:ctx validation/context
                                :gen gen/any
                                :f (constantly false)}))

(defspec validation-applicative-interchange 10
  (lt/applicative-interchange {:ctx validation/context
                               :gen gen/int
                               :appf (validation/ok inc)}))

(defspec validation-applicative-composition 10
  (lt/applicative-composition {:ctx validation/context
                               :gen gen/int
                               :appf (validation/ok inc)
                               :appg (validation/ok dec)}))
