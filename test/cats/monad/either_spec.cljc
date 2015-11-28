(ns cats.monad.either-spec
  #?@(:cljs
      [(:require [clojure.string :as s]
                 [cljs.test :as t]
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
     (:require [clojure.string :as s]
               [clojure.test :as t]
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

;; Functor

(defspec either-first-functor-law 10
  (lt/first-functor-law
   {:gen either-gen}))

(defspec either-second-functor-law 10
  (lt/second-functor-law
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


(def either-vector-m
  (either/either-t b/vector-context))

(t/deftest either-t-tests
  (t/is (= [(either/right 2)]
           (ctx/with-context either-vector-m
             (m/return 2))))

  (t/is (= [(either/right 1)
            (either/right 2)
            (either/right 2)
            (either/right 3)]
           (ctx/with-context either-vector-m
             (m/mlet [x [(either/right 0) (either/right 1)]
                      y [(either/right 1) (either/right 2)]]
               (m/return (+ x y))))))

  (t/is (= [(either/right 1)
            (either/right 2)
            (either/right 2)
            (either/right 3)]
           (ctx/with-context either-vector-m
             (m/mlet [x (m/lift [0 1])
                      y (m/lift [1 2])]
               (m/return (+ x y))))))

  (t/is (= [(either/right 1)
            (either/left)
            (either/right 2)
            (either/left)]
           (ctx/with-context either-vector-m
             (m/mlet [x [(either/right 0) (either/right 1)]
                      y [(either/right 1) (either/left)]]
               (m/return (+ x y)))))))

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
