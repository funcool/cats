;; Copyright (c) 2014-2016 Andrey Antukh <niwi@niwi.nz>
;; Copyright (c) 2014-2016 Alejandro Gómez <alejandro@dialelo.com>
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
  (:require [cats.protocols :as p]
            [cats.context :as ctx]
            [cats.util :as util]))

;; --- Type constructor and functions

(declare context)

(defrecord Right [right]
  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] right)

  p/Printable
  (-repr [_]
    (str "#<Right " (pr-str right) ">"))

  #?@(:cljs [cljs.core/IDeref
             (-deref [_] right)]
      :clj  [clojure.lang.IDeref
             (deref [_] right)]))

(defrecord Left [left]
  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] left)

  p/Printable
  (-repr [_]
    (str "#<Left " (pr-str left) ">"))

  #?@(:cljs [cljs.core/IDeref
             (-deref [_] left)]
      :clj  [clojure.lang.IDeref
             (deref [_] left)]))

(alter-meta! #'->Right assoc :private true)
(alter-meta! #'->Left assoc :private true)

(util/make-printable Right)
(util/make-printable Left)

(defn left
  "A Left type constructor."
  ([] (->Left nil))
  ([v] (->Left v)))

(defn right
  "A Right type constructor."
  ([] (->Right nil))
  ([v] (->Right v)))

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
  (cond
    (or (instance? Right v)
        (instance? Left v))
    true

    (satisfies? p/Contextual v)
    (identical? (p/-get-context v) context)

    :else false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  context
  (reify
    p/Context

    p/Semigroup
    (-mappend [_ sv sv']
      (if (right? sv)
        sv
        sv'))

    p/Functor
    (-fmap [_ f s]
      (if (right? s)
        (right (f (p/-extract ^Right s)))
        s))

    p/Bifunctor
    (-bimap [_ f g s]
      (if (left? s)
        (left  (f (p/-extract ^Left s)))
        (right (g (p/-extract ^Right s)))))

    p/Applicative
    (-pure [_ v]
      (right v))

    (-fapply [m af av]
      (if (right? af)
        (p/-fmap m (p/-extract ^Right af) av)
        af))

    p/Monad
    (-mreturn [_ v]
      (right v))

    (-mbind [_ s f]
      (assert (either? s) (str "Context mismatch: " (p/-repr s)
                               " is not allowed to use with either context."))
      (if (right? s)
        (f (p/-extract ^Right s))
        s))

    p/MonadZero
    (-mzero [_]
      (left))

    p/MonadPlus
    (-mplus [_ mv mv']
      (if (right? mv)
        mv
        mv'))

    p/Foldable
    (-foldl [_ f z mv]
      (if (right? mv)
        (f z (p/-extract mv))
        z))

    (-foldr [_ f z mv]
      (if (right? mv)
        (f (p/-extract mv) z)
        z))

    p/Traversable
    (-traverse [_ f mv]
      (if (right? mv)
        (let [a (f (p/-extract mv))]
          (p/-fmap (p/-get-context a) right a))
        (p/-pure (ctx/infer) mv)))

    p/Printable
    (-repr [_]
      "#<Either>")))

(util/make-printable (type context))

;; --- Utility functions

(defn branch
  "Given an either value and two functions, if the either is a
  left apply the first function to the value it contains; if the
  either is a right apply the second function to its value."
  [e lf rf]
  {:pre [(either? e)]}
  (if (left? e)
    (lf (p/-extract e))
    (rf (p/-extract e))))

(def either branch)

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
  (p/-mbind context e rf))

(def lefts
  "Given a collection of eithers, return only the values that are left."
  (partial filter left?))

(def rights
  "Given a collection of eithers, return only the values that are right."
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
    (right (p/-extract e))
    (left (p/-extract e))))

#?(:clj
   (defmacro try-either
     "Try to evalute the body and return the result as an either.
     If an exception is thrown return the exception as a left,
     otherwise return the result as a right."
     [& body]
     ;; detect compilation of a cljs namespace and inject the appropriate error
     ;; see https://groups.google.com/forum/#!topic/clojurescript/iBY5HaQda4A
     (if (:ns &env)
       `(try (right (do ~@body)) (catch js/Error e# (left e#)))
       `(try (right (do ~@body)) (catch Exception e# (left e#))))))
