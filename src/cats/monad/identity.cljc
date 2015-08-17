;; Copyright (c) 2014-2015 Andrey Antukh <niwi@niwi.nz>
;; Copyright (c) 2014-2015 Alejandro Gómez <alejandro@dialelo.com>
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

(ns cats.monad.identity
  "The Identity Monad."
  (:require [cats.protocols :as p])
  (:refer-clojure :exclude [identity]))

(declare context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Identity [v]
  p/Context
  (get-context [_] context)

  p/Extract
  (extract [_] v)

  #?(:clj clojure.lang.IDeref
     :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_] v)

  #?@(:clj
      [Object
       (equals [self other]
         (if (instance? Identity other)
           (= v (.-v other))
           false))

       (toString [self]
         (str v))])

  #?@(:cljs
      [cljs.core/IEquiv
       (-equiv [_ other]
         (if (instance? Identity other)
           (= v (.-v other))
           false))]))

(alter-meta! #'->Identity assoc :private true)

(defn identity
  "The Identity type constructor."
  [v]
  (Identity. v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  context
  (reify
    p/ContextClass
    (-get-level [_] 10)

    p/Functor
    (fmap [_ f iv]
      (Identity. (f (.-v iv))))

    p/Applicative
    (pure [_ v]
      (Identity. v))

    (fapply [_ af av]
      (Identity. ((.-v af) (.-v av))))

    p/Monad
    (mreturn [_ v]
      (Identity. v))

    (mbind [_ mv f]
      (f (.-v mv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad transformer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn identity-transformer
  "The Identity transformer constructor."
  [inner-monad]
  (reify
    p/ContextClass
    (-get-level [_] 100)

    p/Monad
    (mreturn [_ v]
      (identity (p/mreturn inner-monad v)))

    (mbind [_ mv f]
      nil)))
