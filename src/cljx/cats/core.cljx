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

(ns cats.core
  "Category Theory abstractions for Clojure"
  #+cljs
  (:require-macros [cats.core :refer (with-monad mlet)])
  (:require [cats.protocols :as p])
  (:refer-clojure :exclude [when unless filter sequence]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *context* nil)

#+clj
(defmacro with-monad
  "Set current context to specific monad."
  [ctx & body]
  `(cond
     (satisfies? p/MonadTrans ~ctx)
     (binding [*context* ~ctx]
       ~@body)

     (satisfies? p/MonadTrans *context*)
     (do ~@body)

     :else
     (binding [*context* ~ctx]
       ~@body)))

(defn get-current-context
  "Get current context or obtain it from
  the provided instance."
  ([] (get-current-context nil))
  ([default]
   (cond
     (not (nil? *context*))
     *context*

     (satisfies? p/Context default)
     (p/get-context default)

     (satisfies? p/Monad default)
     default

     :else
     #+clj
     (throw (IllegalArgumentException.
             "You are using return/pure/mzero function without context."))
     #+cljs
     (throw (js/Error.
             "You are using return/pure/mzero function without context.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context-aware funcionts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn pure
  "Given any value v, return it wrapped in
  default/effect free context.

  This is multiarity function that with arity pure/1
  it uses the dynamic scope to resolve the current
  context. With pure/2, you can force a specific context
  value."
  ([v] (pure (get-current-context) v))
  ([ctx v] (p/pure ctx v)))

(defn return
  "This is a monad version of pure."
  ([v] (return (get-current-context) v))
  ([ctx v] (p/mreturn ctx v)))

(defn bind
  "Given a value inside monadic context mv and any function,
  applies a function to value of mv."
  [mv f]
  (cond
    (satisfies? p/MonadTrans *context*)
    (p/mbind *context* mv f)

    (nil? *context*)
    (with-monad (p/get-context mv)
      (p/mbind *context* mv f))

    :else
    (let [ctx (get-current-context mv)]
      (p/mbind ctx mv f))))

(defn mzero
  []
  (let [ctx (get-current-context)]
    (p/mzero ctx)))

(defn mplus
  [& mvs]
  {:pre [(not (empty? mvs))]}
  (let [ctx (get-current-context (first mvs))]
    (reduce (partial p/mplus ctx) mvs)))

(defn guard
  [b]
  (if b
    (return nil)
    (mzero)))

(defn join
  "Remove one level of monadic structure."
  [mv]
  (bind mv identity))

(defn fmap
  "Apply a function f to the value inside functor's fv
  preserving the context type."
  [f fv]
  (p/fmap (get-current-context fv) f fv))

(defn fapply
  "Given function inside af's conext and value inside
  av's context, applies the function to value and return
  a result wrapped in context of same type of av context."
  [af av]
  (p/fapply (get-current-context af) af av))

(defn when
  "If the expression is true, returns the monadic value.

  Otherwise, yields nil in a monadic context."
  ([b mv]
   (when (get-current-context mv) b mv))
  ([ctx b mv]
   (if b
     mv
     (return ctx nil))))

(defn unless
  "If the expression is false, returns the monadic value.

  Otherwise, yields nil in a monadic context."
  [b mv]
  (when-not b
    mv))

(defn lift
  "Lift a value from the inner monad of a monad transformer into a value
  of the monad transformer."
  ([mv]
   (p/lift *context* mv))
  ([m mv]
   (p/lift m mv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monadic Let Macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clj
(defmacro mlet
  "Monad composition macro that works like clojure
  let. This allows much easy composition of monadic
  computations.

  Let see one example for understand how it works, this is
  a code using bind for compose few number of operations:


    (bind (just 1)
          (fn [a]
            (bind (just (inc a))
                  (fn [b]
                    (return (* b 2))))))
    ;=> #<Just [4]>

  Now see how this code can be more clear if you
  are using mlet macro for do it:

    (mlet [a (just 1)
           b (just (inc a))]
      (return (* b 2)))
    ;=> #<Just [4]>
  "
  [bindings & body]
  (when-not (and (vector? bindings)
                 (not-empty bindings)
                 (even? (count bindings)))
    (throw (IllegalArgumentException. "bindings has to be a vector with even number of elements.")))
  (->> (reverse (partition 2 bindings))
       (reduce (fn [acc [l r]]
                 (condp = l
                   :let  `(let ~r ~acc)
                   :when `(bind (guard ~r)
                                (fn [~(gensym)] ~acc))
                   `(bind ~r (fn [~l] ~acc))))
               `(do ~@body))))

(defmacro errlet
  [bindings & body]
  (let [pairs (reverse (partition 2 bindings))]
    (reduce (fn [acc [l r]]
              `(let [c# (atom 0)]
                 (if (satisfies? p/Context ~r)
                   (let [~l (bind ~r (fn [v#]
                                       (swap! c# inc)
                                       (return v#)))]
                     (if (= @c# 0)
                       ~l
                       (with-monad (get-current-context ~l)
                         (let [~l (p/get-value ~l)]
                           ~acc))))
                   (let [~l ~r]
                     ~acc))))
            `(do ~@body)
            pairs)))

(defmacro mlet-with
  "A helper macro for using monad transformers, is the
  same as mlet but specifying the monadic context.
  So, instead of writing:

    (with-monad (maybe-transformer vector-monad)
      (mlet [a [(just 1) (just 2)]
             b [(just (inc a))]]
        (return (* b 2))))
    ;=> [#<Just [4]> #<Just [6]>]

  You can just write:

    (mlet-with (maybe-transformer vector-monad)
      [a [(just 1) (just 2)]
       b [(just (inc a))]]
      (return (* b 2)))
    ;=> [#<Just [4]> #<Just [6]>]
  "
  [monad bindings & body]
  `(with-monad ~monad
     (mlet ~bindings ~@body)))

#+clj
(defmacro lift-m
  "Lifts a function with the given fixed number of arguments to a
  monadic context.

    (require '[cats.monad.maybe :as maybe])
    (require '[cats.core :as m])

    (def monad+ (m/lift-m 2 +))

    (monad+ (maybe/just 1) (maybe/just 2))
    ;=> <Just [3]>

    (monad+ (maybe/just 1) (maybe/nothing))
    ;=> <Nothing>

    (monad+ [0 2 4] [1 2])
    ;=> [1 2 3 4 5 6]
  "
  [n f]
  (let [val-syms (repeatedly n gensym)
        mval-syms (repeatedly n gensym)
        mlet-bindings (interleave val-syms mval-syms)]
    `(fn [~@mval-syms]
       (mlet [~@mlet-bindings]
         (return (~f ~@val-syms))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequences.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sequence
  "Given a non-empty collection of monadic values, collect
  their values in a vector returned in the monadic context.

    (require '[cats.monad.maybe :as maybe])
    (require '[cats.core :as m])

    (m/sequence [(maybe/just 2) (maybe/just 3)])
    ;=> <Just [[2, 3]]>

    (m/sequence [(maybe/nothing) (maybe/just 3)])
    ;=> <Nothing>
  "
  [mvs]
  {:pre [(not-empty mvs)]}
  (let [ctx (get-current-context (first mvs))]
    (with-monad ctx
      (reduce (fn [mvs mv]
                 (mlet [v mv
                        vs mvs]
                       (return (cons v vs))))
              (return '())
              (reverse mvs)))))

(defn mapseq
   "Given a function that takes a value and puts it into a
   monadic context, map it into the given collection
   calling sequence on the results.

     (require '[cats.monad.maybe :as maybe])
     (require '[cats.core :as m])

     (m/mapseq maybe/just [2 3])
     ;=> <Just [[2 3]]>

     (m/mapseq (fn [v]
                  (if (odd? v)
                    (maybe/just v)
                    (maybe/nothing)))
                 [1 2])
     ;=> <Nothing>
  "
  [mf coll]
  (sequence (map mf coll)))

(defn forseq
  "Same as mapseq but with the arguments in reverse order.

    (require '[cats.monad.maybe :as maybe])
    (require '[cats.core :as m])

    (m/forseq [2 3] maybe/just)
    ;=> <Just [[2 3]]>

    (m/forseq [1 2]
              (fn [v]
                (if (odd? v)
                  (maybe/just v)
                  (maybe/nothing))))
    ;=> <Nothing>
  "
  [vs mf]
  (mapseq mf vs))

(defn filter
  "Applies a predicate to a value in a `MonadZero` instance,
  returning the identity element when the predicate yields false.

  Otherwise, returns the instance unchanged.

    (require '[cats.monad.moaybe :as maybe])
    (require '[cats.core :as m])

    (m/filter (partial < 2) (maybe/just 3))
    ;=> <Just [3]>

    (m/filter (partial < 4) (maybe/just 3))
    ;=> <Nothing>
  "
  [p mv]
  (with-monad (get-current-context mv)
    (mlet [v mv
           :when (p v)]
          (return v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell-style aliases and util functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn <$>
  "Alias of fmap."
  ([f]
   (fn [fv]
     (fmap f fv)))
  ([f fv]
   (fmap f fv)))

(defn <*>
  "Performs a Haskell-style left-associative fapply."
  ([af av]
   (fapply af av))
  ([af av & avs]
   (reduce fapply af (cons av avs))))

(defn >>=
  "Performs a Haskell-style left-associative bind.

  Example:
    (>>= (just 1) (comp just inc) (comp just inc))
    ;=> #<Just [3]>
  "
  ([mv f]
   (bind mv f))
  ([mv f & fs]
   (reduce bind mv (cons f fs))))

(defn >>
  "Performs a Haskell-style left-associative bind,
  ignoring the values produced by the monad computations."
  ([mv mv']
   (bind mv (fn [_] mv')))
  ([mv mv' & mvs]
   (reduce >> mv (cons mv' mvs))))

(defn =<<
  "Same as the two argument version of `>>=` but with the
  arguments interchanged."
  [f mv]
  (>>= mv f))

(defn >=>
  "Left-to-right composition of monads."
  [mf mg x]
  (with-monad (get-current-context mf)
    (mlet [a (mf x)
           b (mg a)]
          (return b))))

(defn <=<
  "Right-to-left composition of monads.
  Same as `>=>` with its first two arguments flipped."
  [mg mf x]
  (with-monad (get-current-context mf)
    (mlet [a (mf x)
           b (mg a)]
          (return b))))
