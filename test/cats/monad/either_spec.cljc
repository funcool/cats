(ns cats.monad.either-spec
  #?@(:cljs
      [(:require [clojure.string :as s]
                 [cljs.test :as t]
                 [clojure.test.check]
                 [clojure.test.check.generators :as gen]
                 [clojure.test.check.properties :as prop :include-macros true]
                 [cljs.core.match :refer-macros [match]]
                 [cats.labs.test :as lt]
                 [cats.builtin :as b]
                 [cats.protocols :as p]
                 [cats.monad.maybe :as maybe]
                 [cats.monad.either :as either :include-macros true]
                 [cats.context :as ctx :include-macros true]
                 [cats.core :as m :include-macros true])
       (:require-macros [clojure.test.check.clojure-test :refer (defspec)])])
  #?(:clj
     (:require [clojure.string :as s]
               [clojure.test :as t]
               [clojure.test.check.clojure-test :refer [defspec]]
               [clojure.test.check :as tc]
               [clojure.test.check.generators :as gen]
               [clojure.test.check.properties :as prop]
               [clojure.core.match :refer [match]]
               [cats.labs.test :as lt]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.monad.maybe :as maybe]
               [cats.monad.either :as either]
               [cats.context :as ctx]
               [cats.core :as m])))

;; Generators

(defn rights-of [g]
  (gen/fmap either/right g))

(def right-gen
  (rights-of gen/any))

(def left-gen
  (gen/return (either/left :oh-no)))

(def either-gen
  (gen/one-of [right-gen left-gen]))

(def vectors-gen
  (gen/vector gen/any))

;; Semigroup

(defspec either-semigroup 10
  (lt/semigroup-associativity
   {:ctx either/context
    :gen (rights-of (gen/not-empty vectors-gen))}))

;; Functor

(defspec either-first-functor-law 10
  (lt/first-functor-law
   {:gen either-gen}))

(defspec either-second-functor-law 10
  (lt/second-functor-law
   {:gen either-gen
    :f   str
    :g   count}))

;; Bifunctor

(defspec either-first-identity 10
  (lt/bifunctor-first-identity
   {:gen either-gen}))

(defspec either-second-identity 10
  (lt/bifunctor-second-identity
   {:gen either-gen}))

(defspec either-bimap-identity 10
  (lt/bifunctor-bimap-identity
   {:gen either-gen}))

(defspec either-bifunctor-composition 10
  (lt/bifunctor-composition
   {:gen either-gen
    :f   str
    :g   count}))

;; Applicative

(defspec either-applicative-identity 10
  (lt/applicative-identity-law
   {:ctx either/context
    :gen either-gen}))

(defspec either-applicative-homomorphism 10
  (lt/applicative-homomorphism
   {:ctx either/context
    :gen gen/any
    :f   (constantly false)}))

(defspec either-applicative-interchange 10
  (lt/applicative-interchange
   {:ctx  either/context
    :gen  gen/int
    :appf (either/right inc)}))

(defspec either-applicative-composition 10
  (lt/applicative-composition
   {:ctx  either/context
    :gen  gen/int
    :appf (either/right inc)
    :appg (either/right dec)}))

;; Monad

(defspec either-first-monad-law 10
  (lt/first-monad-law
   {:ctx either/context
    :mf  #(if % (either/right %) (either/left :oh-no))}))

(defspec either-second-monad-law 10
  (lt/second-monad-law {:ctx either/context}))

(defspec either-third-monad-law 10
  (lt/third-monad-law
   {:ctx either/context
    :f   (comp either/right str)
    :g   (comp either/right count)}))

;; MonadPlus

(defspec either-monadplus 10
  (lt/monadplus-associativity
   {:ctx either/context
    :gen (gen/not-empty vectors-gen)}))

;; MonadZero

(defspec either-monadzero-identity 10
  (lt/monadzero-identity-element
   {:ctx either/context
    :gen (rights-of (gen/not-empty vectors-gen))}))

(defspec either-monadzero-bind 10
  (lt/monadzero-bind
   {:ctx  either/context
    :gen  right-gen
    :zero (either/left :oh-no)}))

(t/deftest basic-operations-test
  (t/is (= 1 (m/extract (either/right 1))))
  (t/is (= nil (m/extract (either/left)))))

(t/deftest ideref-test
  (t/is (= 1 @(either/left 1)))
  (t/is (= 1 @(either/right 1))))

(t/deftest predicates-test
  (let [m1 (either/right 1)
        m2 (either/left 42)]
    (t/is (either/either? m1))
    (t/is (either/either? m2))
    (t/is (either/right? m1))
    (t/is (either/left? m2))))

(t/deftest branch-test
  (let [l (either/left "oh no")
        r (either/right 42)]
    (t/is (= "OH NO" (either/branch l s/upper-case inc)))
    (t/is (= 43 (either/branch r s/upper-case inc)))))

(t/deftest branch-left-test
  (let [l (either/left "oh no")
        r (either/right 42)]
    (t/is (= "OH NO" (either/branch-left l s/upper-case)))
    (t/is (= r (either/branch-left r s/upper-case)))))

(t/deftest branch-right-test
  (let [l (either/left "oh no")
        r (either/right 42)]
    (t/is (= l (either/branch-right l inc)))
    (t/is (= 43 (either/branch-right r inc)))))


(t/deftest filtering-test
  (let [l1 (either/left "oh no")
        l2 (either/left "yo ho ho ho")
        r1 (either/right 42)
        r2 (either/right 99)
        es [l1 l2 r1 r2]]
    (t/is (every? either/left? (either/lefts es)))
    (t/is (every? either/right? (either/rights es)))
    (t/is (= l1 (either/first-left es)))
    (t/is (= r1 (either/first-right es)))))

(t/deftest invert-test
  (let [l (either/left "oh no")
        r (either/right "oh no")]
    (t/is (= r (either/invert l)))
    (t/is (= l (either/invert r)))))

(t/deftest foldable-test
  (t/testing "Foldl"
    (t/is (= (either/right 2)
             (m/foldl #(m/return (+ %1 %2)) 1 (either/right 1))))
    (t/is (= 1
             (m/foldl #(m/return (+ %1 %2)) 1 (either/left 5)))))

  (t/testing "Foldr"
    (t/is (= (either/right 2)
             (m/foldr #(m/return (+ %1 %2)) 1 (either/right 1))))
    (t/is (= 1
             (m/foldr #(m/return (+ %1 %2)) 1 (either/left 5))))))

(t/deftest traversable-test
  (t/testing "Traverse"
    (t/is (= (maybe/just (either/right 42))
             (ctx/with-context maybe/context
               (m/traverse #(maybe/just (inc %)) (either/right 41)))))
    (t/is (= (maybe/just (either/left :nope))
             (ctx/with-context maybe/context
               (m/traverse #(maybe/just (inc %)) (either/left :nope)))))))

(t/deftest try-success-test
  (t/testing "try-either for a successful function"
    (let [result (either/try-either (inc 1))]
      (t/is (either/right? result))
      (t/is (= 2 @result)))))

(t/deftest try-exception-test
  (t/testing "try-either for a exceptional function"
    (let [result (either/try-either #?(:clj  (throw (Exception. "oh no!"))
                                       :cljs (throw (js/Error "oh no!"))))]
      (t/is (either/left? result))
      #?(:clj  (t/is (= "oh no!" (.getMessage @result)))
         :cljs (t/is (= "oh no!" (.-message @result))))
      )))

(t/deftest match-test
  (t/testing "Test ILookup"
    (t/is (= (match [(either/left "failure")]
                    [{:left l}] l
                    [{:right r}] r)
             "failure"))
    (t/is (= (match [(either/right 123)]
                    [{:left l}] l
                    [{:right r}] r)
             123))))
