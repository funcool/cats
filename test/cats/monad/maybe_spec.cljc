(ns cats.monad.maybe-spec
  #?@(:cljs
      [(:require [cljs.test :as t]
                 [clojure.test.check]
                 [clojure.test.check.generators :as gen]
                 [clojure.test.check.properties :as prop :include-macros true]
                 [cats.labs.test :as lt]
                 [cats.builtin :as b]
                 [cats.protocols :as p]
                 [cats.monad.maybe :as maybe]
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
               [cats.monad.maybe :as maybe]
               [cats.monad.either :as either]
               [cats.context :as ctx]
               [cats.core :as m])))

;; Generators

(defn justs-of [g]
  (gen/fmap maybe/just g))

(def just-gen
  (justs-of gen/any))

(def nothing-gen
  (gen/return (maybe/nothing)))

(def maybe-gen
  (gen/one-of [just-gen nothing-gen]))

(def vectors-gen
  (gen/vector gen/any))

;; Semigroup

(defspec maybe-semigroup 10
  (lt/semigroup-associativity
   {:ctx maybe/context
    :gen (justs-of (gen/not-empty vectors-gen))}))

;; Monoid

(defspec maybe-monoid 10
  (lt/monoid-identity-element
   {:ctx   maybe/context
    :gen   (justs-of (gen/not-empty vectors-gen))
    :empty (maybe/just [])}))

;; Functor

(defspec maybe-first-functor-law 10
  (lt/first-functor-law {:gen maybe-gen}))

(defspec maybe-second-functor-law 10
  (lt/second-functor-law
   {:gen maybe-gen
    :f   str
    :g   count}))

;; Applicative

(defspec maybe-applicative-identity 10
  (lt/applicative-identity-law
   {:ctx maybe/context
    :gen maybe-gen}))

(defspec maybe-applicative-homomorphism 10
  (lt/applicative-homomorphism
   {:ctx maybe/context
    :gen gen/any
    :f   (constantly false)}))

(defspec maybe-applicative-interchange 10
  (lt/applicative-interchange
   {:ctx  maybe/context
    :gen  gen/int
    :appf (maybe/just inc)}))

(defspec maybe-applicative-composition 10
  (lt/applicative-composition
   {:ctx  maybe/context
    :gen  gen/int
    :appf (maybe/just inc)
    :appg (maybe/just dec)}))

;; Monad

(defspec maybe-first-monad-law 10
  (lt/first-monad-law
   {:ctx maybe/context
    :mf  #(if % (maybe/just %) (maybe/nothing))}))

(defspec maybe-second-monad-law 10
  (lt/second-monad-law {:ctx maybe/context}))

(defspec maybe-third-monad-law 10
  (lt/third-monad-law
   {:ctx maybe/context
    :f   (comp maybe/just str)
    :g   (comp maybe/just count)}))

;; MonadPlus

(defspec maybe-monadplus 10
  (lt/monadplus-associativity
   {:ctx maybe/context
    :gen (gen/not-empty vectors-gen)}))

;; MonadZero

(defspec maybe-monadzero-identity 10
  (lt/monadzero-identity-element
   {:ctx maybe/context
    :gen (justs-of (gen/not-empty vectors-gen))}))

(defspec maybe-monadzero-bind 10
  (lt/monadzero-bind
   {:ctx maybe/context
    :gen just-gen}))

;; Examples

(t/deftest maybe-monad-tests
  (t/testing "Basic maybe operations."
    (t/is (= 1 (maybe/from-maybe (maybe/just 1))))
    (t/is (= 1 (maybe/from-maybe (maybe/just 1) 42)))
    (t/is (= nil (maybe/from-maybe (maybe/nothing))))
    (t/is (= 42 (maybe/from-maybe (maybe/nothing) 42))))

  (t/testing "extract function"
    (t/is (= (m/extract (maybe/just 1)) 1))
    (t/is (= (m/extract (maybe/nothing)) nil)))

  (t/testing "Test IDeref"
    (t/is (= nil @(maybe/nothing)))
    (t/is (= 1 @(maybe/just 1))))

  (t/testing "Test predicates"
    (let [m1 (maybe/just 1)]
      (t/is (maybe/maybe? m1))
      (t/is (maybe/just? m1)))))

(def maybe-vector-t (maybe/maybe-t b/vector-context))

(t/deftest maybe-t-tests
  (t/testing "It can be combined with the effects of other monads"
    (t/is (= [(maybe/just 2)]
             (ctx/with-context maybe-vector-t
               (m/return 2))))

    (t/is (= [(maybe/just 42)]
             (m/with-monad maybe-vector-transformer
               (m/fapply [(maybe/just inc)] [(maybe/just 41)]))))

    (t/is (= [(maybe/just 42)
              (maybe/just 99)
              (maybe/just "41")
              (maybe/just "98")]
             (m/with-monad maybe-vector-transformer
               (m/fapply [(maybe/just inc) (maybe/just str)]
                         [(maybe/just 41) (maybe/just 98)]))))

    (t/is (= [(maybe/just 1)
              (maybe/just 2)
              (maybe/just 2)
              (maybe/just 3)]
             (ctx/with-context maybe-vector-t
               (m/mlet [x [(maybe/just 0) (maybe/just 1)]
                        y [(maybe/just 1) (maybe/just 2)]]
                 (m/return (+ x y))))))

    (t/is (= [(maybe/just 1)
              (maybe/just 2)
              (maybe/just 2)
              (maybe/just 3)]
             (ctx/with-context maybe-vector-t
               (m/mlet [x (m/lift [0 1])
                        y (m/lift [1 2])]
                 (m/return (+ x y))))))

    (t/is (= [(maybe/just 1)
              (maybe/nothing)
              (maybe/just 2)
              (maybe/nothing)]
             (ctx/with-context maybe-vector-t
               (m/mlet [x [(maybe/just 0) (maybe/just 1)]
                        y [(maybe/just 1) (maybe/nothing)]]
                 (m/return (+ x y))))))))

(t/deftest maybe-test
  (let [n (maybe/nothing)
        j (maybe/just 42)]
    (t/is (= 42 (maybe/maybe 42 n inc)))
    (t/is (= 43 (maybe/maybe 42 j inc)))))

(t/deftest seq-conversion-test
  (let [n (maybe/nothing)
        j (maybe/just 42)]
    (t/is (= n (maybe/seq->maybe [])))
    (t/is (= j (maybe/seq->maybe [42 99])))
    (t/is (= [] (maybe/maybe->seq n)))
    (t/is (= [42] (maybe/maybe->seq j)))))

(t/deftest cat-maybes-test
  (let [n1 (maybe/nothing)
        n2 (maybe/nothing)
        j1 (maybe/just 42)
        j2 (maybe/just 99)
        ms [n1 n2 j1 j2]]
    (t/is (= [42 99] (maybe/cat-maybes ms)))))

(t/deftest map-maybe-test
  (let [just-evens #(if (even? %) (maybe/just %) (maybe/nothing))]
    (t/is (= [42 100] (maybe/map-maybe just-evens [41 42 99 100])))))

(t/deftest foldable-test
  (t/testing "Foldl"
    (t/is (= (maybe/just 2)
             (m/foldl #(m/return (+ %1 %2)) 1 (maybe/just 1))))
    (t/is (= 1
             (m/foldl #(m/return (+ %1 %2)) 1 (maybe/nothing)))))

  (t/testing "Foldr"
    (t/is (= (maybe/just 2)
             (m/foldr #(m/return (+ %1 %2)) 1 (maybe/just 1))))
    (t/is (= 1
             (m/foldr #(m/return (+ %1 %2)) 1 (maybe/nothing))))))

(t/deftest traversable-test
  (t/testing "Traverse"
    (t/is (= (either/right (maybe/just 42))
             (ctx/with-context either/context
               (m/traverse #(either/right (inc %)) (maybe/just 41)))))
    (t/is (= (either/right (maybe/nothing))
             (ctx/with-context either/context
               (m/traverse #(either/right (inc %)) (maybe/nothing)))))))
