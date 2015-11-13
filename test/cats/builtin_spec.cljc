(ns cats.builtin-spec
  #?@(:cljs
      [(:require [cljs.test :as t]
                 [clojure.test.check]
                 [clojure.test.check.generators :as gen]
                 [clojure.test.check.properties :as prop :include-macros true]
                 [cats.labs.test :as lt]
                 [cats.builtin :as b]
                 [cats.monad.maybe :as maybe]
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
               [cats.monad.maybe :as maybe]
               [cats.context :as ctx]
               [cats.core :as m])))

(t/deftest test-nil-as-maybe
  (t/testing "Nil works like Nothing (for avoiding unnecesary null pointers)."
    (t/is (= (m/>>= nil (fn [_] (m/return 1))) nil))
    (t/is (= (m/fmap inc nil) nil))
    (t/is (maybe/nothing? nil))
    (t/is (maybe/maybe? nil)))

  (t/testing "extract function"
    (t/is (= (m/extract nil) nil))))

;; Map

(def map-gen (gen/map gen/keyword gen/any))

(defspec map-semigroup 10
  (lt/semigroup-associativity
   {:ctx b/map-context
    :gen map-gen}))

(defspec map-monoid 10
  (lt/monoid-identity-element
   {:ctx b/map-context
    :gen map-gen}))

(defspec map-first-functor-law 10
  (lt/first-functor-law
   {:gen map-gen}))

(defspec map-second-functor-law 10
  (lt/second-functor-law
   {:gen map-gen
    :f   (fn [[k v]] [k (str k v)])
    :g   (fn [[k v]] [k (count v)])}))

(t/deftest array-map-foldable
  (t/testing "Foldl"
    (t/is (= {:a 1 :b 2 :c 3}
             (m/foldl (fn [acc [k v]] (merge acc {k v}))
                      {} {:a 1 :b 2 :c 3})))
    (t/is (= #{"a:1" "b:2" "c:3" "d:4"}
             (m/foldl (fn [acc [k v]] (conj acc (str (name k) ":" v)))
                      #{} {:a 1 :b 2 :c 3 :d 4})))
    (t/is (= 6 (m/foldl (fn [acc [k v]] (+ acc v)) 0 {:a 1 :b 2 :c 3}))))

  (t/testing "Foldr"
    (t/is (= {:a 1 :b 2 :c 3}
             (m/foldr (fn [[k v] acc] (merge {k v} acc))
                      {} {:a 1 :b 2 :c 3})))
    (t/is (= #{"a:1" "b:2" "c:3" "d:4"}
             (m/foldr (fn [[k v] acc] (conj acc (str (name k) ":" v)))
                      #{} {:a 1 :b 2 :c 3 :d 4})))
    (t/is (= 6 (m/foldr (fn [[k v] acc] (+ acc v)) 0 {:a 1 :b 2 :c 3})))))


;; Vector

(defspec vector-semigroup 10
  (lt/semigroup-associativity
   {:ctx b/vector-context
    :gen (gen/not-empty (gen/vector gen/any))}))

(defspec vector-monoid 10
  (lt/monoid-identity-element
   {:ctx b/vector-context
    :gen (gen/vector gen/any)}))

(defspec vector-first-functor-law 10
  (lt/first-functor-law
   {:gen (gen/vector gen/any)}))

(defspec vector-second-functor-law 10
  (lt/second-functor-law
   {:gen (gen/vector gen/any)
    :f   vector
    :g   vector}))

(defspec vector-applicative-identity 10
  (lt/applicative-identity-law
   {:ctx b/vector-context
    :gen (gen/vector gen/any)}))

(defspec vector-applicative-homomorphism 10
  (lt/applicative-homomorphism
   {:ctx b/vector-context
    :gen gen/any
    :f   (constantly false)}))

(defspec vector-applicative-interchange 10
  (lt/applicative-interchange
   {:ctx  b/vector-context
    :gen  gen/int
    :appf [inc]}))

(defspec vector-applicative-composition 10
  (lt/applicative-composition
   {:ctx  b/vector-context
    :gen  gen/int
    :appf [inc]
    :appg [dec]}))

(defspec vector-first-monad-law 10
  (lt/first-monad-law
   {:ctx b/vector-context
    :mf  #(if % (vector %) [])}))

(defspec vector-second-monad-law 10
  (lt/second-monad-law {:ctx b/vector-context}))

(defspec vector-third-monad-law 10
  (lt/third-monad-law
   {:ctx b/vector-context
    :f   (comp vector str)
    :g   (comp vector count)}))

(t/deftest vector-foldable
  (t/testing "Foldl"
    (t/is (= [3 2 1] (m/foldl (fn [acc v] (into [v] acc)) [] [1 2 3])))
    (t/is (= 6 (m/foldl + 0 [1 2 3]))))

  (t/testing "Foldr"
    (t/is (= [1 2 3] (m/foldr (fn [v acc] (into [v] acc)) [] [1 2 3])))
    (t/is (= 6 (m/foldr + 0 [1 2 3])))))

(defn inc-if-even
  [n]
  (if (even? n)
    (maybe/just (inc n))
    (maybe/nothing)))

(t/deftest vector-traversable
  (t/testing "Traverse"
    (t/is (= (maybe/just [])
             (ctx/with-context maybe/context
               (m/traverse inc-if-even []))))
    (t/is (= (maybe/just [3 5])
             (ctx/with-context maybe/context
               (m/traverse inc-if-even [2 4]))))
    (t/is (= (maybe/nothing)
             (ctx/with-context maybe/context
               (m/traverse inc-if-even [1 2]))))))

;; Sequence

(defn sequence-gen [g]
  (gen/fmap #(lazy-seq %) (gen/vector g)))

(defspec sequence-semigroup 10
  (lt/semigroup-associativity
   {:ctx b/sequence-context
    :gen (gen/not-empty (sequence-gen gen/any))}))

(defspec sequence-monoid 10
  (lt/monoid-identity-element
   {:ctx b/sequence-context
    :gen (sequence-gen gen/any)}))

(defspec sequence-first-functor-law 10
  (lt/first-functor-law
   {:gen (sequence-gen gen/any)}))

(defspec sequence-second-functor-law 10
  (lt/second-functor-law
   {:gen (sequence-gen gen/any)
    :f   #(lazy-seq [%])
    :g   #(lazy-seq [%])}))

(defspec sequence-applicative-identity 10
  (lt/applicative-identity-law
   {:ctx b/sequence-context
    :gen (sequence-gen gen/any)}))

(defspec sequence-applicative-homomorphism 10
  (lt/applicative-homomorphism
   {:ctx b/sequence-context
    :gen gen/any
    :f   (constantly false)}))

(defspec sequence-applicative-interchange 10
  (lt/applicative-interchange
   {:ctx  b/sequence-context
    :gen  gen/int
    :appf (lazy-seq [inc])}))

(defspec sequence-applicative-composition 10
  (lt/applicative-composition
   {:ctx  b/sequence-context
    :gen  gen/int
    :appf (lazy-seq [inc])
    :appg (lazy-seq [dec])}))

(defspec sequence-first-monad-law 10
  (lt/first-monad-law
   {:ctx b/sequence-context
    :mf  #(if % (lazy-seq [%]) (lazy-seq []))}))

(defspec sequence-second-monad-law 10
  (lt/second-monad-law {:ctx b/sequence-context}))

(defspec sequence-third-monad-law 10
  (lt/third-monad-law
   {:ctx b/sequence-context
    :f   (comp seq vector str)
    :g   (comp seq vector count)}))

(t/deftest lazyseq-foldable
  (t/testing "Foldl"
    (t/is (= [3 2 1]
             (m/foldl (fn [acc v] (into [v] acc)) [] (map identity [1 2 3]))))
    (t/is (= 6 (m/foldl + 0 (map identity [1 2 3])))))

  (t/testing "Foldr"
    (t/is (= [1 2 3]
             (m/foldr (fn [v acc] (into [v] acc)) [] (map identity [1 2 3]))))
    (t/is (= 6 (m/foldr + 0 (map identity [1 2 3]))))))

(t/deftest lazyseq-traversable
  (t/testing "Traverse"
    (t/is (= (maybe/just [])
             (ctx/with-context maybe/context
               (m/traverse inc-if-even (lazy-seq [])))))
    (t/is (= (maybe/just [3 5])
             (ctx/with-context maybe/context
               (m/traverse inc-if-even (lazy-seq [2 4])))))
    (t/is (= (maybe/nothing)
             (ctx/with-context maybe/context
               (m/traverse inc-if-even (lazy-seq [1 2])))))))

;; Range

(t/deftest range-foldable
  (t/testing "Foldl"
    (t/is (= [3 2 1] (m/foldl (fn [acc v] (into [v] acc)) [] (range 1 4))))
    (t/is (= 6 (m/foldl + 0 (range 1 4)))))

  (t/testing "Foldr"
    (t/is (= [1 2 3] (m/foldr (fn [v acc] (into [v] acc)) [] (range 1 4))))
    (t/is (= 6 (m/foldr + 0 (range 1 4))))))

;; Set

;; fixme: test.check 0.9.0 will include the `set` generator
(defn set-gen
  [g]
  (gen/fmap set (gen/vector g)))

(defspec set-semigroup 10
  (lt/semigroup-associativity
   {:ctx b/set-context
    :gen (gen/not-empty (set-gen gen/any))}))

(defspec set-monoid 10
  (lt/monoid-identity-element
   {:ctx b/set-context
    :gen (set-gen gen/any)}))

(defspec set-first-functor-law 10
  (lt/first-functor-law {:gen (set-gen gen/any)}))

(defspec set-second-functor-law 10
  (lt/second-functor-law
   {:gen (set-gen gen/any)
    :f   (comp set vector)
    :g   (comp set vector)}))

(defspec set-applicative-identity 10
  (lt/applicative-identity-law
   {:ctx b/set-context
    :gen (set-gen gen/any)}))

(defspec set-applicative-homomorphism 10
  (lt/applicative-homomorphism
   {:ctx b/set-context
    :gen gen/any
    :f   (constantly false)}))

(defspec set-applicative-interchange 10
  (lt/applicative-interchange
   {:ctx  b/set-context
    :gen  gen/int
    :appf #{inc}}))

(defspec set-applicative-composition 10
  (lt/applicative-composition
   {:ctx  b/set-context
    :gen  gen/int
    :appf #{inc}
    :appg #{dec}}))

(defspec set-first-monad-law 10
  (lt/first-monad-law
   {:ctx b/set-context
    :mf  #(if % #{%} #{})}))

(defspec set-second-monad-law 10
  (lt/second-monad-law {:ctx b/set-context}))

(defspec set-third-monad-law 10
  (lt/third-monad-law
   {:ctx b/set-context
    :f   (comp set vector str)
    :g   (comp set vector count)}))

;; Function

(def fn-gen
  (gen/one-of [(gen/return inc) (gen/return dec)]))

(defn fn-eq
  ([f g]
   (= (f 42)
      (g 42)))
  ([f g h]
   (= (f 42)
      (g 42)
      (h 42))))

(defspec fn-semigroup 10
  (lt/semigroup-associativity
   {:ctx b/function-context
    :gen fn-gen
    :eq  fn-eq}))

(defspec fn-monoid 10
  (lt/monoid-identity-element
   {:ctx b/function-context
    :gen fn-gen
    :eq  fn-eq}))

(defspec fn-first-functor-law 10
  (lt/first-functor-law
   {:gen fn-gen
    :eq  fn-eq}))

(defspec fn-second-functor-law 10
  (lt/second-functor-law
   {:gen fn-gen
    :eq  fn-eq
    :f   #(+ % 2)
    :g   #(- % 2)}))

(defspec fn-applicative-identity 10
  (lt/applicative-identity-law
   {:ctx b/function-context
    :gen fn-gen
    :eq  fn-eq}))

(defspec fn-applicative-homomorphism 10
  (lt/applicative-homomorphism
   {:ctx b/function-context
    :eq  fn-eq
    :gen gen/int
    :f   inc}))

(defspec fn-applicative-interchange 10
  (lt/applicative-interchange
   {:ctx  b/function-context
    :eq   fn-eq
    :gen  gen/int
    :appf (constantly inc)}))


(defspec fn-applicative-composition 10
  (lt/applicative-composition
   {:ctx  b/function-context
    :eq   fn-eq
    :gen  gen/int
    :appf (constantly inc)
    :appg (constantly dec)}))

(defspec fn-first-monad-law 10
  (lt/first-monad-law
   {:ctx b/function-context
    :gen fn-gen
    :mf  (constantly (constantly 42))
    :eq  fn-eq}))

(defspec fn-second-monad-law 10
  (lt/second-monad-law
   {:ctx b/function-context
    :eq  fn-eq}))

(defspec fn-third-monad-law 10
  (lt/third-monad-law
   {:ctx b/function-context
    :eq  fn-eq
    :f   (constantly str)
    :g   (constantly vector)}))

;; Any

(defspec any-semigroup 10
  (lt/semigroup-associativity
   {:ctx b/any-monoid
    :gen gen/boolean}))

(defspec any-monoid 10
  (lt/monoid-identity-element
   {:ctx b/any-monoid
    :gen gen/boolean}))

;; All

(defspec all-semigroup 10
  (lt/semigroup-associativity
   {:ctx b/all-monoid
    :gen gen/boolean}))

(defspec all-monoid 10
  (lt/monoid-identity-element
   {:ctx b/all-monoid
    :gen gen/boolean}))

;; Sum

(defspec sum-semigroup 10
  (lt/semigroup-associativity
   {:ctx b/sum-monoid
    :gen gen/int}))

(defspec sum-monoid 10
  (lt/monoid-identity-element
   {:ctx b/sum-monoid
    :gen gen/int}))

;; Prod

(defspec prod-semigroup 10
  (lt/semigroup-associativity
   {:ctx b/prod-monoid
    :gen gen/int}))

(defspec prod-monoid 10
  (lt/monoid-identity-element
   {:ctx b/prod-monoid
    :gen gen/int}))

;; String

(defspec string-semigroup 10
  (lt/semigroup-associativity
   {:ctx b/string-monoid
    :gen gen/string}))

(defspec string-monoid 10
  (lt/monoid-identity-element
   {:ctx b/string-monoid
    :gen gen/string}))
