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

(ns cats.monad.either
  "The Either (Error) Monad."
  (:require [cats.protocols :as proto]))

(declare either-monad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructor and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Either [v type]
  proto/Context
  (get-context [_]
    either-monad)

  #+clj
  Object
  #+clj
  (equals [self other]
    (if (instance? Either other)
      (and (= v (.-v other))
         (= type (.-type other)))
      false))

  #+clj
  (toString [self]
    (with-out-str (print [v type])))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [self other]
    (if (instance? Either other)
      (and (= v (.-v other))
           (= type (.-type other)))
      false)))

(defn either?
  [v]
  (instance? Either v))

(defn left
  "Left constructor for Either type."
  ([v]
     (Either. v :left))
  ([]
     (Either. nil :left)))

(defn right
  "Right constructor for Either type."
  ([v]
     (Either. v :right))
  ([]
     (Either. nil :right)))

(defn left?
  [mv]
  (= (.-type mv) :left))

(defn right?
  [mv]
  (= (.-type mv) :right))

(defn from-either
  "Return inner value of either monad."
  [mv]
  (.-v mv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def either-monad
 (reify
   proto/Functor
   (fmap [_ f s]
     (if (right? s)
       (right (f (.-v s)))
       s))

   proto/Applicative
   (pure [_ v]
     (right v))

   (fapply [m af av]
     (if (right? af)
       (proto/fmap m (.-v af) av)
       af))

   proto/Monad
   (mreturn [_ v]
     (right v))

   (mbind [_ s f]
     (if (right? s)
       (f (.-v s))
       s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad transformer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn either-trans [inner-m]
  (reify
    proto/Monad
    (mreturn [_ v]
      (proto/mreturn inner-m (right v)))

    (mbind [_ mv f]
      (proto/mbind inner-m
                   mv
                   (fn [either-v]
                     (if (left? either-v)
                       (proto/mreturn inner-m either-v)
                       (f (from-either either-v))))))

    proto/MonadTrans
    (inner [_]
      inner-m)

    (lift [m mv]
      (proto/mbind inner-m
                   mv
                   (fn [v]
                     (proto/mreturn inner-m (right v)))))))
