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

(ns cats.monad.either
  "The Either monad implementation and helper functions
  for working with either related types.

  Also commonly known as Error monad.

      (require '[cats.monad.either :as either])

      (either/right 1)
      ;; => #<Right [1]>

      (either/left 1)
      ;; => #<Left [1]>
  "
  (:require [cats.protocols :as proto]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructor and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare either-monad)

(deftype Right [v]
  proto/Context
  (get-context [_] either-monad)

  proto/Extract
  (extract [_] v)

  #?(:clj clojure.lang.IDeref
     :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_] v)

  #?@(:clj
      [Object
       (equals [self other]
         (if (instance? Right other)
           (= v (.-v other))
           false))

       (toString [self]
         (with-out-str (print [v])))])

  #?@(:cljs
       [cljs.core/IEquiv
        (-equiv [_ other]
                (if (instance? Right other)
                  (= v (.-v other))
                  false))]))

(deftype Left [v]
  proto/Context
  (get-context [_] either-monad)

  proto/Extract
  (extract [_] v)

  #?(:clj clojure.lang.IDeref
     :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_] v)

  #?@(:clj
      [Object
       (equals [self other]
         (if (instance? Left other)
           (= v (.-v other))
           false))

       (toString [self]
         (with-out-str (print [v])))])

  #?@(:cljs
       [cljs.core/IEquiv
        (-equiv [_ other]
                (if (instance? Left other)
                  (= v (.-v other))
                  false))]))

(alter-meta! #'->Right assoc :private true)
(alter-meta! #'->Left assoc :private true)

(defn left
  "A Left type constructor."
  ([] (Left. nil))
  ([v] (Left. v)))

(defn right
  "A Right type constructor."
  ([] (Right. nil))
  ([v] (Right. v)))

(defn left?
  "Return true if `v` is an instance
  of Left type."
  [v]
  (instance? Left v))

(defn right?
  "Return true if `v` is an instance
  of Right type."
  [v]
  (instance? Right v))

(defn either?
  "Return true in case of `v` is instance
  of Either monad."
  [v]
  (if (satisfies? proto/Context v)
    (identical? (proto/get-context v) either-monad)
    false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  either-monad
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

(defn either-transformer
  "The Either transformer constructor."
  [inner-monad]
  (reify
    proto/Monad
    (mreturn [_ v]
      (proto/mreturn inner-monad (right v)))

    (mbind [_ mv f]
      (proto/mbind inner-monad
                   mv
                   (fn [either-v]
                     (if (left? either-v)
                       (proto/mreturn inner-monad either-v)
                       (f (proto/extract either-v))))))

    proto/MonadTrans
    (base [_]
      either-monad)

    (inner [_]
      inner-monad)

    (lift [m mv]
      (proto/mbind inner-monad
                   mv
                   (fn [v]
                     (proto/mreturn inner-monad (right v)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn branch
  "Given an either value and two functions, if the either is a
  left apply the first function to the value it contains; if the
  either is a right apply the second function to its value."
  [e lf rf]
  {:pre [(either? e)]}
  (if (left? e)
    (lf (proto/extract e))
    (rf (proto/extract e))))

(defn branch-left
  "Given an either value and a function, if the either is a
  left, apply the function to the value it contains; if the
  either is a right, return it."
  [e lf]
  {:pre [(either? e)]}
  (branch e lf right))

(defn branch-right
  "Either-specific synonym for #'cats.core/bind

  Given an either value and a function, if the either is a
  right, apply the function to the value it contains; if the
  either is a left, return it."
  [e rf]
  {:pre [(either? e)]}
  (let [context either-monad]
    (proto/mbind context e rf)))

(def lefts
  "Given a collection of eithers, return only the values that are left."
  (partial filter left?))

(def rights
  "Given a collection of eithers, return only the values that are left."
  (partial filter right?))

(def first-left
  "Given a collection of either, return the first value that is left"
  (comp first lefts))

(def first-right
  "Given a collection of either, return the first value that is right"
  (comp first rights))

(defn invert
  "Convert a left to a right or viceversa, preserving content."
  [e]
  {:pre [(either? e)]}
  (if (left? e)
    (right (proto/extract e))
    (left (proto/extract e))))
