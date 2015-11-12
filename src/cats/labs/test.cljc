;; Copyright (c) 2015 Andrey Antukh <niwi@niwi.nz>
;; Copyright (c) 2015 Alejandro Gómez <alejandro@dialelo.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(ns cats.labs.test
  (:require [cats.context :as ctx :include-macros true]
            [cats.core :as m]
            [cats.protocols :as p]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop :include-macros true]))

;; Generator context

(def gen-context
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Functor
    (-fmap [_ f mv]
      (gen/fmap f mv))

    p/Applicative
    (-pure [_ v]
      (gen/return v))
    (-fapply [_ gf gv]
      (m/mlet [f gf
               v gv]
        (m/return (f v))))

    p/Monad
    (-mreturn [_ v]
      (gen/return v))

    (-mbind [_ mv f]
      (gen/bind mv f))))

(extend-type clojure.test.check.generators.Generator
  p/Contextual
  (-get-context [_] gen-context))

;; Semigroup

(defn semigroup-associativity
  [{:keys [ctx gen eq] :or {eq =}}]
  (prop/for-all [x gen
                 y gen
                 z gen]
    (ctx/with-context ctx
      (eq (m/mappend (m/mappend x y) z)
          (m/mappend x (m/mappend y z))))))

;; Monoid

(defn monoid-identity-element
  [{:keys [ctx gen empty eq] :or {empty (m/mempty ctx) eq =}}]
  (prop/for-all [x gen]
    (ctx/with-context ctx
      (eq x
          (m/mappend x empty)
          (m/mappend empty x)))))

;; Functor laws

(defn first-functor-law
  [{:keys [gen eq] :or {eq =}}]
  (prop/for-all [fa gen]
    (eq fa
        (m/fmap identity fa))))

(defn second-functor-law
  [{:keys [gen f g eq] :or {eq =}}]
  (prop/for-all [fa gen]
    (eq (m/fmap (comp g f) fa)
        (m/fmap g (m/fmap f fa)))))

;; Applicative laws

(defn applicative-identity-law
  [{:keys [ctx gen eq] :or {eq =}}]
  (prop/for-all [app gen]
    (eq app
        (m/fapply (m/pure ctx identity) app))))

(defn applicative-homomorphism
  [{:keys [ctx gen f eq] :or {eq =}}]
  (prop/for-all [x gen]
    (eq (m/pure ctx (f x))
        (m/fapply (m/pure ctx f) (m/pure ctx x)))))

(defn applicative-interchange
  [{:keys [ctx gen appf eq] :or {eq =}}]
  (prop/for-all [x gen]
    (eq (m/fapply appf (m/pure ctx x))
        (m/fapply (m/pure ctx (fn [f] (f x))) appf))))

(defn applicative-composition
  [{:keys [ctx gen appf appg eq] :or {eq =}}]
  (prop/for-all [x gen]
    (eq (m/fapply appg
                  (m/fapply appf (m/pure ctx x)))
        (m/fapply (m/pure ctx (m/curry 2 comp))
                  appf
                  appg
                  (m/pure ctx x)))))

;; Monad laws

(defn first-monad-law
  [{:keys [ctx mf gen eq] :or {gen gen/any eq =}}]
  (prop/for-all [a gen]
    (eq (mf a)
        (m/>>= (m/return ctx a) mf))))

(defn second-monad-law
  [{:keys [ctx eq] :or {eq =}}]
  (prop/for-all [a gen/any]
    (let [m (m/return ctx a)]
      (eq m
          (m/>>= m m/return)))))

(defn third-monad-law
  [{:keys [ctx f g eq] :or {eq =}}]
  (prop/for-all [a gen/any]
    (let [m (m/return ctx a)]
      (eq (m/>>= (m/>>= m f) g)
          (m/>>= m (fn [x] (m/>>= (f x) g)))))))

;; MonadPlus

(defn monadplus-associativity
  [{:keys [ctx gen eq] :or {eq =}}]
  (prop/for-all [x gen
                 y gen
                 z gen]
    (ctx/with-context ctx
      (eq (m/mplus (m/mplus x y) z)
          (m/mplus x (m/mplus y z))))))

;; MonadZero

(defn monadzero-identity-element
  [{:keys [ctx gen eq] :or {eq =}}]
  (prop/for-all [x gen]
    (ctx/with-context ctx
      (eq x
          (m/mplus x (m/mzero ctx))
          (m/mplus (m/mzero ctx) x)))))

(defn monadzero-bind
  [{:keys [ctx gen zero eq] :or {zero (m/mzero ctx) eq =}}]
  (prop/for-all [m gen]
    (ctx/with-context ctx
      (eq zero
          (m/>>= zero (fn [v] (m/return v)))
          (m/>> m zero)))))
