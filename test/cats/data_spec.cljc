(ns cats.data-spec
  (:require [clojure.test.check]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop #?@(:cljs [:include-macros true])]
            [cats.labs.test :as lt]
            [cats.data :as d]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.context :as ctx #?@(:cljs [:include-macros true])]
            [#?(:clj clojure.test :cljs cljs.test) :as t]
            [cats.core :as m #?@(:cljs [:include-macros true])]
            #?(:clj [clojure.test.check.clojure-test :refer [defspec]]))
  #?(:cljs (:require-macros [clojure.test.check.clojure-test :refer (defspec)])))

(def ctx (d/pair-monoid b/string-monoid))

(defn pair-gen [g]
  (m/alet [s1 g, s2 g]
    (d/pair s1 s2)))

(defspec pair-semigroup 10
  (lt/semigroup-associativity
   {:ctx ctx
    :gen (pair-gen gen/string)}))

(defspec pair-monoid 10
  (lt/monoid-identity-element
   {:ctx   ctx
    :gen   (pair-gen gen/string)
    :empty (d/pair "" "")}))

(defspec pair-monoid-sum 10
  (lt/monoid-identity-element
   {:ctx   (d/pair-monoid b/sum-monoid)
    :gen   (pair-gen gen/int)
    :empty (d/pair 0 0)}))

(defspec pair-first-functor-law 10
  (lt/first-functor-law {:gen (pair-gen gen/any)}))

(defspec pair-second-functor-law 10
  (lt/second-functor-law
   {:gen (pair-gen gen/any)
    :f   str
    :g   count}))

;; Bifunctor

(defspec pair-first-identity 10
  (lt/bifunctor-first-identity
   {:gen (pair-gen gen/any)}))

(defspec pair-second-identity 10
  (lt/bifunctor-second-identity
   {:gen (pair-gen gen/any)}))

(defspec pair-bimap-identity 10
  (lt/bifunctor-bimap-identity
   {:gen (pair-gen gen/any)}))

(defspec pair-bifunctor-composition 10
  (lt/bifunctor-composition
   {:gen (pair-gen gen/any)
    :f   str
    :g   count}))

(t/deftest pair-foldable
  (t/testing "Foldl"
    (t/is (= (/ 1 3) (m/foldl / 1 (d/pair 0 3)))))

  (t/testing "Foldr"
    (t/is (= (/ 3 1) (m/foldr / 1 (d/pair 0 3))))))

(defn inc-if-even [n]
  (if (even? n)
    (maybe/just (inc n))
    (maybe/nothing)))

(t/deftest pair-traversable
  (t/testing "Traverse"
    (t/is (= (maybe/just (d/pair 0 3))
             (ctx/with-context maybe/context
               (m/traverse inc-if-even (d/pair 0 2)))))
    (t/is (= (maybe/nothing)
             (ctx/with-context maybe/context
               (m/traverse inc-if-even (d/pair 0 1)))))))
