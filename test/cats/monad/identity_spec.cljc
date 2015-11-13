(ns cats.monad.identity-spec
  #?@(:cljs
      [(:require [cljs.test :as t]
                 [clojure.test.check]
                 [clojure.test.check.generators :as gen]
                 [clojure.test.check.properties :as prop :include-macros true]
                 [cats.labs.test :as lt]
                 [cats.builtin :as b]
                 [cats.monad.identity :as id]
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
               [cats.monad.identity :as id]
               [cats.context :as ctx]
               [cats.core :as m])))

(t/deftest basic-operations-test
  (t/is (= 1 @(id/identity 1))))

(def id-gen
  (gen/fmap id/identity gen/any))

(defspec identity-first-functor-law 10
  (lt/first-functor-law {:gen id-gen}))

(defspec identity-second-functor-law 10
  (lt/second-functor-law
   {:gen id-gen
    :f   str
    :g   count}))

(defspec identity-applicative-identity 10
  (lt/applicative-identity-law
   {:ctx id/context
    :gen id-gen}))

(defspec identity-applicative-homomorphism 10
  (lt/applicative-homomorphism
   {:ctx id/context
    :gen gen/any
    :f   str}))

(defspec identity-applicative-interchange 10
  (lt/applicative-interchange
   {:ctx  id/context
    :gen  gen/int
    :appf (id/identity inc)}))

(defspec identity-applicative-composition 10
  (lt/applicative-composition
   {:ctx  id/context
    :gen  gen/int
    :appf (id/identity inc)
    :appg (id/identity dec)}))

(defspec identity-first-monad-law 10
  (lt/first-monad-law
   {:ctx id/context
    :mf  (comp id/identity str)}))

(defspec identity-second-monad-law 10
  (lt/second-monad-law {:ctx id/context}))

(defspec identity-third-monad-law 10
  (lt/third-monad-law
   {:ctx id/context
    :f   (comp id/identity str)
    :g   (comp id/identity count)}))
