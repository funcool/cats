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

(ns cats.core
  "Category Theory abstractions for Clojure"
  #?(:cljs
     (:require-macros [cats.core :refer (with-monad mlet)]))
  (:require [cats.protocols :as p])
  (:refer-clojure :exclude [when unless filter sequence]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:dynamic true
       :no-doc true}
  *context* nil)

#?(:clj
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
         ~@body))))

(defn ^{:no-doc true}
  get-current-context
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
     (throw (#?(:clj IllegalArgumentException.
                :cljs js/Error.)
             "You are using return/pure/mzero function without context.")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context-aware funcionts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mempty
  []
  (let [ctx (get-current-context)]
    (p/mempty ctx)))

(defn mappend
  [& svs]
  {:pre [(not (empty? svs))]}
  (let [ctx (get-current-context (first svs))]
    (reduce (partial p/mappend ctx) svs)))

(defn pure
  "Given any value v, return it wrapped in
  default/effect free context.

  This is multiarity function that with arity pure/1
  it uses the dynamic scope to resolve the current
  context. With `pure/2`, you can force a specific context
  value.

  Example:

      (with-monad either/either-monad
        (pure 1)
      ;; => #<Right [1]>
  "
  ([v] (pure (get-current-context) v))
  ([ctx v] (p/pure ctx v)))

(defn return
  "This is a monad version of pure and it works
  identically to it."
  ([v] (return (get-current-context) v))
  ([ctx v] (p/mreturn ctx v)))

(defn bind
  "Given a value inside monadic context `mv` and any function,
  applies a function to value of mv.

      (bind (either/right 1) (fn [v]
                               (return (inc v))))
      ;; => #<Right [2]>

  For convenience, you may prefer use a `mlet` macro
  that add a beautiful, let like syntax for
  compose operations with `bind` function."
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
  ([]
   (let [ctx (get-current-context)]
     (p/mzero ctx)))
  ([ctx]
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
  "Remove one level of monadic structure.
  This is same as that `(bind mv identity)`"
  [mv]
  (bind mv identity))

(defn fmap
  "Apply a function f to the value inside functor's fv
  preserving the context type."
  ([f]
   (fn [fv]
     (fmap f fv)))
  ([f fv]
   (-> (get-current-context fv)
       (p/fmap f fv))))

(defn fapply
  "Given function inside af's context and value inside
  av's context, applies the function to value and return
  a result wrapped in context of same type of av context.

  This function is variadic, so it can be used like
  a haskell style left-associative fapply."
  [af & avs]
  {:pre [(not (empty? avs))]}
  (let [ctx (get-current-context af)]
    (reduce (partial p/fapply ctx) af avs)))

(defn when
  "If the expression is true, returns the monadic value.
  Otherwise, yields nil in a monadic context."
  ([b mv]
   (when (get-current-context mv) b mv))
  ([ctx b mv]
   (if b mv (return ctx nil))))

(defn unless
  "If the expression is false, returns the monadic value.
  Otherwise, yields nil in a monadic context."
  [b mv]
  (when-not b
    mv))

(defn lift
  "Lift a value from the inner monad of a monad transformer
  into a value of the monad transformer."
  ([mv] (p/lift *context* mv))
  ([m mv] (p/lift m mv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monadic Let Macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#?(:clj
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
                   (case l
                     :let  `(let ~r ~acc)
                     :when `(bind (guard ~r)
                                  (fn [~(gensym)] ~acc))
                     `(bind ~r (fn [~l] ~acc))))
                 `(do ~@body)))))

(defn- arglists
  [var]
  (get (meta var) :arglists))

(defn- single-arity?
  [var]
  (let [args (arglists var)]
    (and (= (count args) 1)
         (not (some #{'&} (first args))))))

(defn- arity
  [var]
  {:pre [(single-arity? var)]}
  (count (first (arglists var))))

#?(:clj
   (defmacro curry*
     [args body]
     (let [argcount (count args)]
       (cond
         (= argcount 0) `(fn f# [] ~body)
         (= argcount 1) `(fn f#
                           ([] f#)
                           ([~@args] ~body))
         :else
         (let [arities (for [n (range 1 (count args))]
                         `([~@(take n args)] (curry* ~(drop n args) ~body)))]
           `(fn f#
              ([] f#)
              ~@arities
              ([~@args] ~body)))
         ))))

#?(:clj
   (defmacro curry
    "Given either a fixed arity function or an arity and a function
    yields another which is curried.

    ;; Inferred arity (function must have one fixed arity)

    (defn add2 [x y] (+ x y))
    (def cadd2 (curry add2))

    ((cadd2 1) 3)
    ;; => 4

    (cadd2 1 3)
    ;; => 4

    ;; Fixed arity

    (def c+ (curry 3 +))

    ((c+ 1 2) 3)
    ;; => 6

    ((((c+) 1) 2) 3)
    ;; => 6
    "
    ([f]
     (if (not (symbol? f))
       (throw (IllegalArgumentException. "You must provide an arity for currying anonymous functions"))
       (let [fvar (resolve f)]
         (if-let [args (arglists fvar)]
           (if (single-arity? fvar)
             `(curry ~(arity fvar) ~f)
             (throw (IllegalArgumentException. "The given function is either variadic or has multiple arities, provide an arity for currying.")))
           (throw (IllegalArgumentException. "The given function doesn't have arity metadata, provide an arity for currying."))))))
    ([n f]
     (let [args (repeatedly n gensym)
           body `(~f ~@args)]
       `(curry* ~args ~body)))))

;; TODO: infer arity when possible
#?(:clj
   (defmacro lift-m
    "Lifts a function with the given fixed number of arguments to a
    monadic context.

        (def monad+ (lift-m 2 +))

        (monad+ (maybe/just 1) (maybe/just 2))
        ;; => <Just [3]>

        (monad+ (maybe/just 1) (maybe/nothing))
        ;; => <Nothing>

        (monad+ [0 2 4] [1 2])
        ;; => [1 2 3 4 5 6]
    "
    [n f]
    (let [val-syms (repeatedly n gensym)
          mval-syms (repeatedly n gensym)
          mlet-bindings (interleave val-syms mval-syms)]
      `(fn [~@mval-syms]
         (mlet [~@mlet-bindings]
           (return (~f ~@val-syms)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequences.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sequence
  "Given a non-empty collection of monadic values, collect
  their values in a vector returned in the monadic context.

      (sequence [(maybe/just 2) (maybe/just 3)])
      ;; => <Just [[2, 3]]>

      (sequence [(maybe/nothing) (maybe/just 3)])
      ;; => <Nothing>
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

  Let se a little example:

      (m/forseq [2 3] maybe/just)
      ;; => <Just [[2 3]]>

  Yet an other example that fails:

      (m/forseq [1 2]
                (fn [v]
                  (if (odd? v)
                    (maybe/just v)
                    (maybe/nothing))))
      ;; => <Nothing>
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

(def <$>
  "A haskell-style fmap alias."
  fmap)

(def <*>
  "A haskell-style fapply alias."
  fapply)

(defn >>=
  "Performs a Haskell-style left-associative
  bind.

  Let see it in action:

      (>>= (just 1) (comp just inc) (comp just inc))
      ;; => #<Just [3]>
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

(defn extract
  "Generic function for unwrap/extract
  the inner value of a container."
  [v]
  (p/extract v))

(def <> mappend)
