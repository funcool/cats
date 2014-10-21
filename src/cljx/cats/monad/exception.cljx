;; Copyright (c) 2014, Andrey Antukh
;; Copyright (c) 2014, Alejandro Gómez
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

(ns cats.monad.exception
  "The Try Monad.

  The Try type represents a computation that may either result in an exception,
  or return a successfully computed value. It's similar to, but semantically
  different from the Either type."
  (:require [cats.protocols :as proto])

  #+cljs
  (:require-macros [cats.monad.exception :refer [try-on]]))

(declare exception-monad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Success and Failure Types definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Success [v]
  proto/Context
  (get-context [_] exception-monad)
  (get-value [_] v)

  #+clj
  Object
  #+clj
  (equals [self other]
    (if (instance? Success other)
      (= v (.-v other))
      false))

  #+clj
  (toString [self]
    (with-out-str (print [v])))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [self other]
    (if (instance? Success other)
      (= v (.-v other))
      false)))


(deftype Failure [e]
  proto/Context
  (get-context [_] exception-monad)
  (get-value [_] e)

  #+clj
  Object
  #+clj
  (equals [self other]
    (if (instance? Failure other)
      (= e (.-e other))
      false))

  #+clj
  (toString [self]
    (with-out-str (print [e])))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [self other]
    (if (instance? Failure other)
      (= e (.-e other))
      false)))

(defn success
  [v]
  (Success. v))

(defn failure
  [e]
  (Failure. e))

(defn success?
  [v]
  (instance? Success v))

(defn failure?
  [v]
  (instance? Failure v))

(defn try?
  [v]
  (or (success? v)
      (failure? v)))

#+clj
(defmacro try-on
  "Wraps a computation and return success of failure."
  [expr]
  `(try
     (let [r# ~expr]
       (success r#))
     (catch Throwable e#
       (failure e#))))

(defn from-success
  [sv]
  (.-v sv))

(defn from-failure
  [fv]
  (.-e fv))

(defn from-try
  [v]
  (cond
   (success? v)
   (from-success v)

   (failure? v)
   (from-failure v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def exception-monad
 (reify
   proto/Functor
   (fmap [_ f s]
     (if (success? s)
       (try-on (f (.-v s)))
       s))

   proto/Applicative
   (pure [_ v]
     (success v))

   (fapply [m af av]
     (if (success? af)
       (proto/fmap m (.-v af) av)
       af))

   proto/Monad
   (mreturn [_ v]
     (success v))

   (mbind [_ s f]
     (if (success? s)
       (f (.-v s))
       s))))

