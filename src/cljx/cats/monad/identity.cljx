;; Copyright (c) 2014-2015 Andrey Antukh <niwi@niwi.be>
;; Copyright (c) 2014-2015 Alejandro Gómez
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
  (:require [cats.protocols :as proto])
  (:refer-clojure :exclude [identity]))

(declare identity-monad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Identity [v]
  proto/Context
  (get-context [_] identity-monad)
  (get-value [_] v)

  #+clj
  clojure.lang.IDeref
  #+clj
  (deref [_] v)

  #+cljs
  IDeref
  #+cljs
  (-deref [_] v)

  #+clj
  Object
  #+clj
  (equals [_ other]
    (= v (.-v other)))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [_ other]
    (= v (.-v other)))

  #+clj
  (toString [_]
    (str v)))

(defn identity
  "Default constructor for identity type."
  [v]
  (Identity. v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def identity-monad
  (reify
    proto/Functor
    (fmap [_ f iv]
      (Identity. (f (.-v iv))))

    proto/Applicative
    (pure [_ v]
      (Identity. v))

    (fapply [_ af av]
      (Identity. ((.-v af) (.-v av))))

    proto/Monad
    (mreturn [_ v]
      (Identity. v))

    (mbind [_ mv f]
      (f (.-v mv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad transformer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn identity-transformer [inner-monad]
  (reify
    proto/Monad
    (mreturn [_ v]
      (identity (proto/mreturn inner-monad v)))

    (mbind [_ mv f]
      nil)

))
