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

(ns cats.core
  "Category Theory abstractions for Clojure"
  #?(:cljs
     (:require-macros [cats.core :refer (mlet alet)]))
  #?(:cljs
     (:require [cats.protocols :as p]
               [clojure.set]
               [cats.context :as ctx :include-macros true])
     :clj
     (:require [cats.protocols :as p]
               [clojure.set]
               [cats.context :as ctx]))
  (:refer-clojure :exclude [filter sequence unless when]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context-aware functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn mempty
  ([] (p/-mempty (ctx/infer)))
  ([ctx] (p/-mempty ctx)))

(defn mappend
  [& svs]
  {:pre [(seq svs)]}
  (let [ctx (ctx/infer (first svs))]
    (reduce (partial p/-mappend ctx) svs)))

(defn pure
  "Given any value `v`, return it wrapped in
  the default/effect-free context.

  This is a multi-arity function that with arity `pure/1`
  uses the dynamic scope to resolve the current
  context. With `pure/2`, you can force a specific context
  value.

  Example:

      (with-context either/context
        (pure 1))
      ;; => #<Right [1]>

      (pure either/context 1)
      ;; => #<Right [1]>
  "
  ([v] (pure (ctx/infer) v))
  ([ctx v] (p/-pure ctx v)))

(defn return
  "This is a monad version of `pure` and works
  identically to it."
  ([v] (return (ctx/infer) v))
  ([ctx v] (p/-mreturn ctx v)))

(defn bind
  "Given a monadic value `mv` and a function `f`,
  apply `f` to the unwrapped value of `mv`.

      (bind (either/right 1) (fn [v]
                               (return (inc v))))
      ;; => #<Right [2]>

  For convenience, you may prefer to use the `mlet` macro,
  which provides a beautiful, `let`-like syntax for
  composing operations with the `bind` function."
  [mv f]
  (let [ctx (ctx/infer mv)]
    (p/-mbind ctx mv (fn [v]
                       (ctx/with-context ctx
                         (f v))))))

(defn mzero
  ([]
   (p/-mzero (ctx/infer)))
  ([ctx]
   (p/-mzero ctx)))

(defn mplus
  [& mvs]
  {:pre [(seq mvs)]}
  (let [ctx (ctx/infer (first mvs))]
    (reduce (partial p/-mplus ctx) mvs)))

(defn guard
  [b]
  (if b
    (return true)
    (mzero)))

(defn join
  "Remove one level of monadic structure.
  This is the same as `(bind mv identity)`."
  [mv]
  (bind mv identity))

(defn fmap
  "Apply a function `f` to the value wrapped in functor `fv`,
  preserving the context type."
  ([f]
   (fn [fv]
     (fmap f fv)))
  ([f fv]
   (let [ctx (ctx/infer fv)]
     (ctx/with-context ctx
       (p/-fmap ctx f fv)))))

(defn bimap
  "Map over both arguments at the same time.

  Given functions `f` and `g` and a value wrapped in a bifunctor `bv`,
  apply `f` to a first argument or `g` to a second argument.

      (bimap dec inc (either/right 1)
      ;; => #<Right 2>

      (bimap dec inc (either/left 1)
      ;; => #<Left 0>"
  ([f g]
   (fn [bv]
     (bimap f g bv)))
  ([f g bv]
   (let [ctx (ctx/infer bv)]
     (ctx/with-context ctx
       (p/-bimap ctx f g bv)))))

(defn left-map
  "Map covariantly over the first argument.

  Given a function `f` and a value wrapped in a bifunctor `bv`,
  apply `f` to the first argument, if present, otherwise leave `bv` unchanged.

      (left-map dec (either/right 1)
      ;; => #<Right 1>

      (left-map dec (either/left 1)
      ;; => #<Left 0>"
  ([f]
   (fn [bv]
     (left-map f bv)))
  ([f bv]
   (bimap f identity bv)))

(defn right-map
  "Map covariantly over the second argument.

  Given a function `g` and a value wrapped in a bifunctor `bv`,
  apply `g` to the second argument, if present, otherwise leave `bv` unchanged.

      (right-map inc (either/right 1)
      ;; => #<Right 2>

      (right-map inc (either/left 1)
      ;; => #<Left 1>"
  ([g]
   (fn [bv]
     (right-map g bv)))
  ([g bv]
   (bimap identity g bv)))

(defn fapply
  "Given a function wrapped in a monadic context `af`,
  and a value wrapped in a monadic context `av`,
  apply the unwrapped function to the unwrapped value
  and return the result, wrapped in the same context as `av`.

  This function is variadic, so it can be used like
  a Haskell-style left-associative fapply."
  [af & avs]
  {:pre [(seq avs)]}
  (let [ctx (ctx/infer af)]
    (reduce (partial p/-fapply ctx) af avs)))

(defn when
  "Given an expression and a monadic value,
  if the expression is logical true, return the monadic value.
  Otherwise, return nil in a monadic context."
  ([b mv]
   (when (ctx/infer mv) b mv))
  ([ctx b mv]
   (if b
     mv
     (pure ctx nil))))

(defn unless
  "Given an expression and a monadic value,
  if the expression is not logical true, return the monadic value.
  Otherwise, return nil in a monadic context."
  ([b mv]
   (when (not b) mv))
  ([ctx b mv]
   (when ctx (not b) mv)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monadic Let Macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#?(:clj
   (defmacro mlet
     "Monad composition macro that works like Clojure's
     `let`. This facilitates much easier composition of
     monadic computations.

     Let's see an example to understand how it works.
     This code uses bind to compose a few operations:

         (bind (just 1)
               (fn [a]
                 (bind (just (inc a))
                         (fn [b]
                           (return (* b 2))))))
         ;=> #<Just [4]>

     Now see how this code can be made clearer
     by using the mlet macro:

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applicative Let Macro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- deps
  [expr syms]
  (cond
    (and (symbol? expr)
         (contains? syms expr))
    (list expr)

    (seq? expr)
    (mapcat #(deps % syms) expr)

    :else
    '()))

(defn- rename-sym
  [expr renames]
  (get renames expr expr))

(defn- rename
  [expr renames]
  (cond
    (symbol? expr)
    (rename-sym expr renames)
    (seq? expr)
    (map #(rename % renames) expr)
    :else
    expr))

(defn- dedupe-symbols*
  [sym->ap body]
  (letfn [(renamer [{:keys [body syms aps seen renames] :as summ} [s ap]]
           (let [ap' (rename ap renames)
                 new-aps (conj aps ap')]
             (if (seen s)
               (let [s' (gensym)
                     new-syms (conj syms s')
                     new-seen (conj seen s')
                     new-renames (assoc renames s s')
                     new-body (rename body new-renames)]
                 {:syms new-syms
                  :aps new-aps
                  :seen new-seen
                  :renames new-renames
                  :body new-body})
               (let [new-syms (conj syms s)
                     new-seen (conj seen s)]
                 {:syms new-syms
                  :aps new-aps
                  :seen new-seen
                  :renames renames
                  :body body}))))]
    (let [summ
          (reduce renamer
                  {:syms []
                   :aps []
                   :seen #{}
                   :renames {}
                   :body body}
                  sym->ap)]
      [(mapv vector (:syms summ) (:aps summ)) (:body summ)])))

(defn- dedupe-symbols
  [bindings body]
  (let [syms (map first bindings)
        aps (map second bindings)
        sym->ap (mapv vector syms aps)]
    (dedupe-symbols* sym->ap body)))

(defn- dependency-map
  [sym->ap]
  (let [syms (map first sym->ap)
        symset (set syms)]
    (into []
          (for [[s ap] sym->ap
                :let [ds (set (deps ap symset))]]
            [s ds]))))

(defn- remove-deps
  [deps symset]
  (let [removed (for [[s depset] deps]
                  [s (clojure.set/difference depset symset)])]
    (into (empty deps) removed)))

(defn- topo-sort*
  [deps seen batches current]
  (if (empty? deps)
    (conj batches current)
    (let [dep (first deps)
          [s dependencies] dep
          dependant? (some dependencies seen)]
      (if (nil? dependant?)
        (recur (subvec deps 1)
               (conj seen s)
               batches
               (conj current s))
        (recur (remove-deps (subvec deps 1) (set current))
               (conj seen s)
               (conj batches current)
               [s])))))

(defn- topo-sort
  [deps]
  (let [syms (into #{} (map first deps))]
    (topo-sort* deps #{} [] [])))

(defn- bindings->batches
  [bindings]
  (let [syms (map first bindings)
        aps (map second bindings)
        sym->ap (mapv vector syms aps)
        sorted-deps (topo-sort (dependency-map sym->ap))]
    sorted-deps))

(defn- alet*
  [batches env body]
  (let [fb (first batches)
        rb (rest batches)
        fs (first fb)
        fa (get env fs)
        code
        (reduce (fn [acc syms]
                  (let [fs (first syms)
                        fa (get env fs)
                        rs (rest syms)
                        faps (map #(get env %) rs)]
                    (if (= (count syms) 1)
                      `(fmap (fn [~fs] ~acc) ~fa)
                      (let [cf (reduce (fn [f sym] `(fn [~sym] ~f))
                                       acc
                                       (reverse syms))]
                        `(fapply (fmap ~cf ~fa) ~@faps)))))
                `(do ~@body)
                (reverse batches))
        join-count (dec (count batches))]
    (reduce (fn [acc _]
            `(join ~acc))
        code
        (range join-count))))

#?(:clj
   (defmacro alet
     "Applicative composition macro similar to Clojure's
     `let`. This macro facilitates composition of applicative
     computations using `fmap` and `fapply` and evaluating
     applicative values in parallel.

     Let's see an example to understand how it works.
     This code uses fmap for executing computations inside
     an applicative context:

       (fmap (fn [a] (inc a)) (just 1))
       ;=> #<Just [2]>

     Now see how this code can be made clearer
     by using the alet macro:

       (alet [a (just 1)]
         (inc a))
       ;=> #<Just [2]>

     Let's look at a more complex example, imagine we have
     dependencies between applicative values:

       (join
         (fapply
          (fmap
            (fn [a]
              (fn [b]
                (fmap (fn [c] (inc c))
                      (just (+ a b)))))
            (just 1))
          (just 2)))
       ;=> #<Just [4]>

     This is greatly simplified using `alet`:

       (alet [a (just 1)
              b (just 2)
              c (just (+ a b))]
         (inc c))
      ;=> #<Just [4]>

     The intent of the code is much clearer and evaluates `a` and `b`
     at the same time, then proceeds to evaluate `c` when all the values
     it depends on are available. This evaluation strategy is specially
     helpful for asynchronous applicatives."
     [bindings & body]
     (when-not (and (vector? bindings)
                    (not-empty bindings)
                    (even? (count bindings)))
       (throw (IllegalArgumentException. "bindings has to be a vector with even number of elements.")))
     (let [bindings (partition 2 bindings)
           [bindings body] (dedupe-symbols bindings body)
           batches (bindings->batches bindings)
           env (into {} bindings)]
       (if (and (= (count batches) 1)
                (= (count (map first bindings)) 1))
         `(fmap (fn [~@(map first bindings)]
                  ~@body)
                ~@(map second bindings))
         (alet* batches env body)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; applicative "idiomatic apply"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ap
  "Apply a pure function to applicative arguments, e.g.

   (ap + (just 1) (just 2) (just 3))
   ;; => #<Just [6]>
   (ap str [\"hi\" \"lo\"] [\"bye\" \"woah\" \"hey\"])
   ;; => [\"hibye\" \"hiwoah\" \"hihey\"
          \"lobye\" \"lowoah\" \"lohey\"]

   `ap` is essentially sugar for `(apply fapply (pure f) args)`,
   but for the common case where you have a pure, uncurried,
   possibly variadic function.

   `ap` actually desugars in `alet` form:

   (macroexpand-1 `(ap + (just 1) (just2)))
   ;; => (alet [a1 (just 1) a2 (just 2)] (+ a1 a2))

   That way, variadic functions Just Work, without needing to specify
   an arity separately.

   If you're familiar with Haskell, this is closest to writing
   \"in Applicative style\": you can straightforwardly convert
   pure function application to effectful application by with
   some light syntax (<$> and <*> in case of Haskell, and `ap` here).

   See the original Applicative paper for more inspiration:
   http://staff.city.ac.uk/~ross/papers/Applicative.pdf"
  [f & args]
  (let [syms (repeatedly (count args) (partial gensym "arg"))]
    `(alet [~@(interleave syms args)]
        (~f ~@syms))))

(defmacro ap->
  "Thread like `->`, within an applicative idiom.

  Compare:

  (macroexpand-1 `(-> a b c (d e f)))
  => (d (c (b a) e f)

  with:

  (macroexpand-1 `(ap-> a b c (d e f))
  => (ap d (ap c (ap b a) e f))
  "
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(ap ~(first form) ~x ~@(next form)) (meta form))
                       `(ap ~form ~x))]
        (recur threaded (next forms)))
      x)))

(defmacro ap->>
  "Thread like `->>`, within an applicative idiom.
   See `cats.labs.sugar/ap->` for more in-depth discussion."
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(ap ~(first form) ~@(next form)  ~x) (meta form))
                       `(ap ~form ~x))]
        (recur threaded (next forms)))
      x)))

(defmacro as-ap->
  "Thread like `as->`, within an applicative idiom.
   See `cats.labs.sugar/ap->` for more in-depth discussion."
  [expr name & forms]
  `(let [~name ~expr
         ~@(interleave (repeat name) (for [form forms] `(ap ~@form)))]
     ~name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monadic arrow macros.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro ->=
  "Like `->`, but with monadic binding instead of pure application.
   A mnemonic for the name is a pun on `>>=`, the monadic bind operator,
   and clojure's regular arrow macros.

   You can think of it as generalizing the `some->` thread macro
   to all Monads instead of just Maybe.

   Alternatively, if you think of the regular thread macro as
   sugar for `let`:

   (-> :a b (c (other args)) d)
   =>
   (let [res (b :a)
         res (c res (other args))
         res (d res)]
     res)

   Then `->=` is sugar for cats.core/mlet:

   (->= m-a b (c (other args)) d)
   (mlet [res m-a
          res (c res (other args))
          res (d res)]
     (return res))

   Note that extra args in this context are assumed pure, and will
   be evaluated along with the function itself; this also matches
   the behavior of `some->` wrt extra args.

   Threading through pure functions is somewhat awkward, but can be done:

   (->= m-a
        monadic-fn
        (-> pure-fn
            other-pure-fn
            m/return)
        other-monadic-fn)"
  [expr & forms]
  (let [g (gensym)
        pstep (fn [step] `(-> ~g ~step))]
    `(mlet [~g ~expr
              ~@(interleave (repeat g) (map pstep forms))]
           (return ~g))))

(defmacro ->>=
  "Like ->>, but with monadic binding instead of pure application.
   See `cats.labs.sugar/->=` for more in-depth discussion."
  [expr & forms]
  (let [g (gensym)
        pstep (fn [step] `(->> ~g ~step))]
    `(mlet [~g ~expr
              ~@(interleave (repeat g) (map pstep forms))]
           (return ~g))))

(defmacro as->=
  "Like `as->`, but with monadic binding instead of pure application.
   See `cats.labs.sugar/->=` for more in-depth discussion."
  [expr name & forms]
  `(mlet [~name ~expr
            ~@(interleave (repeat name) forms)]
     (return ~name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Curry Facilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
              ([~@args] ~body)))))))

#?(:clj
   (defmacro curry
     "Given either a fixed arity function or an arity and a function,
     return another which is curried.

     With inferred arity (function must have one fixed arity):

         (defn add2 [x y] (+ x y))
         (def cadd2 (curry add2))

         ((cadd2 1) 3)
         ;; => 4

         (cadd2 1 3)
         ;; => 4

     With given arity:

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
      {:pre [(< n 21)]}
      (let [args (repeatedly n gensym)
            body `(~f ~@args)]
        `(curry* ~args ~body)))))

#?(:clj
   (defmacro lift-m
     "Lift a function with a given fixed arity to a monadic context.

         (def monad+ (lift-m 2 +))

         (monad+ (maybe/just 1) (maybe/just 2))
         ;; => <Just [3]>

         (monad+ (maybe/just 1) (maybe/nothing))
         ;; => <Nothing>

         (monad+ [0 2 4] [1 2])
         ;; => [1 2 3 4 5 6]
     "
     ([f]
      (if (not (symbol? f))
        (throw (IllegalArgumentException.
                "You must provide an arity for lifting anonymous functions"))
        (let [fvar (resolve f)]
          (if-let [args (arglists fvar)]
            (if (single-arity? fvar)
              `(lift-m ~(arity fvar) ~f)
              (throw (IllegalArgumentException.
                      "The given function is either variadic or has multiple arities, provide an arity for lifting.")))
            (throw (IllegalArgumentException.
                    "The given function doesn't have arity metadata, provide an arity for lifting."))))))
     ([n f]
      (let [val-syms (repeatedly n gensym)
            mval-syms (repeatedly n gensym)
            mlet-bindings (interleave val-syms mval-syms)]
        `(fn [~@mval-syms]
           (mlet [~@mlet-bindings]
             (return (~f ~@val-syms))))))))

#?(:clj
   (defmacro lift-a
     "Lift a function with a given fixed arity to an applicative context.

         (def app+ (lift-a 2 +))

         (app+ (maybe/just 1) (maybe/just 2))
         ;; => <Just 3>

         (app+ (maybe/just 1) (maybe/nothing))
         ;; => <Nothing>

         (app+ [0 2 4] [1 2])
         ;; => [1 2 3 4 5 6]
     "
     ([f]
      (if (not (symbol? f))
        (throw (IllegalArgumentException.
                "You must provide an arity for lifting anonymous functions"))
        (let [fvar (resolve f)]
          (if-let [args (arglists fvar)]
            (if (single-arity? fvar)
              `(lift-a ~(arity fvar) ~f)
              (throw (IllegalArgumentException.
                      "The given function is either variadic or has multiple arities, provide an arity for lifting.")))
            (throw (IllegalArgumentException.
                    "The given function doesn't have arity metadata, provide an arity for lifting."))))))
     ([n f]
      (let [val-syms (repeatedly n gensym)
            mval-syms (repeatedly n gensym)
            bindings (interleave val-syms mval-syms)]
        `(fn [~@mval-syms]
           (alet [~@bindings]
             (~f ~@val-syms)))))))

#?(:clj
   (defmacro curry-lift-m
     "Composition of `curry` and `lift-m`"
     [n f]
     `(curry ~n (lift-m ~n ~f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequences.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn sequence
  "Given a collection of monadic values, collect
  their values in a seq returned in the monadic context.

      (require '[cats.context :as ctx]
               '[cats.monad.maybe :as maybe]
               '[cats.core :as m])

      (m/sequence [(maybe/just 2) (maybe/just 3)])
      ;; => #<Just [[2, 3]]>

      (m/sequence [(maybe/nothing) (maybe/just 3)])
      ;; => #<Nothing>

      (ctx/with-context maybe/context
        (m/sequence []))
      ;; => #<Just [()]>
  "
  [mvs]
  (if (empty? mvs)
    (return ())
    (let [ctx (ctx/infer (first mvs))]
      (ctx/with-context ctx
        (reduce (fn [mvs mv]
                  (mlet [v mv
                         vs mvs]
                    (return (cons v vs))))
                (return ())
                (reverse mvs))))))

(defn mapseq
  "Given a function `mf` that takes a value and puts it into a
  monadic context, and a collection, map `mf` over the collection,
  calling `sequence` on the results.

      (require '[cats.context :as ctx]
               '[cats.monad.maybe :as maybe]
               '[cats.core :as m])

      (m/mapseq maybe/just [2 3])
      ;=> <Just [[2 3]]>

      (m/mapseq (fn [v]
                  (if (odd? v)
                    (maybe/just v)
                    (maybe/nothing)))
                [1 2])
      ;; => #<Nothing>

      (ctx/with-context maybe/context
        (mapseq #(maybe/just (* % 2)) []))
      ;; => #<Just [()]>
  "
  [mf coll]
  (sequence (map mf coll)))

(defn forseq
  "Same as `mapseq` but with the arguments flipped.

  Let's see a little example:

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
  "Apply a predicate to a value in a `MonadZero` instance,
  returning the identity element when the predicate does not hold.

  Otherwise, return the instance unchanged.

      (require '[cats.monad.maybe :as maybe])
      (require '[cats.core :as m])

      (m/filter (partial < 2) (maybe/just 3))
      ;=> <Just [3]>

      (m/filter (partial < 4) (maybe/just 3))
      ;=> <Nothing>
  "
  [p mv]
  (mlet [v mv
         :when (p v)]
    (return v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Haskell-style aliases and util functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def <$>
  "A Haskell-style `fmap` alias."
  fmap)

(def <*>
  "A Haskell-style `fapply` alias."
  fapply)

(defn >>=
  "Perform a Haskell-style left-associative bind.

  Let's see it in action:

      (>>= (just 1) (comp just inc) (comp just inc))
      ;; => #<Just [3]>
  "
  ([mv f]
   (bind mv f))
  ([mv f & fs]
   (reduce bind mv (cons f fs))))

(defn >>
  "Perform a Haskell-style left-associative bind,
  ignoring the values produced by the monadic computations."
  ([mv mv']
   (bind mv (fn [_] mv')))
  ([mv mv' & mvs]
   (reduce >> mv (cons mv' mvs))))

(defn =<<
  "Same as the two argument version of `>>=` but with the
  arguments flipped."
  [f mv]
  (>>= mv f))

(defn >=>
  "Left-to-right composition of monads."
  [mf mg x]
  (ctx/with-context (ctx/infer mf)
    (mlet [a (mf x)
           b (mg a)]
      (return b))))

(defn <=<
  "Right-to-left composition of monads.
  Same as `>=>` with its first two arguments flipped."
  [mg mf x]
  (ctx/with-context (ctx/infer mf)
    (mlet [a (mf x)
           b (mg a)]
      (return b))))

(defn extract
  "Generic function to unwrap/extract
  the inner value of a container."
  [v]
  (p/-extract v))

(def <> mappend)

(defn foldr
  "Perform a right-associative fold on the data structure."
  [f z xs]
  (let [ctx (p/-get-context xs)]
    (ctx/with-context ctx
      (p/-foldr ctx f z xs))))

(defn foldl
  "Perform a left-associative fold on the data structure."
  [f z xs]
  (let [ctx (p/-get-context xs)]
    (ctx/with-context ctx
      (p/-foldl ctx f z xs))))

(defn foldm
  "Given an optional monadic context, a function that takes two non-monadic
  arguments and returns a value inside the given monadic context, an initial
  value, and a collection of values, perform a left-associative fold.

      (require '[cats.context :as ctx]
               '[cats.core :as m]
               '[cats.monad.maybe :as maybe])

      (defn m-div [x y]
        (if (zero? y)
          (maybe/nothing)
          (maybe/just (/ x y))))

      (m/foldm m-div 1 [1 2 3])
      (m/foldm maybe/context m-div 1 [1 2 3])
      ;; => #<Just 1/6>

      (m/foldm maybe/context m-div 1 [1 0 3])
      ;; => #<Nothing>

      (foldm m-div 1 [])
      ;; => Exception

      (m/foldm maybe/context m-div 1 [])
      (ctx/with-context maybe/context
        (foldm m-div 1 []))
      ;; => #<Just 1>
  "
  ([f z xs]
   (if (empty? xs)
     (return z)
     (let [[h & t] xs]
       (mlet [z' (f z h)]
         (if (empty? t)
           (return z')
           (foldm f z' t))))))
  ([ctx f z xs]
   (if (empty? xs)
     (return ctx z)
     (foldm f z xs))))

(defn traverse
  "Map each element of a structure to an action, evaluate these
  actions from left to right, and collect the results.

      (defn inc-if-even
        [n]
        (if (even? n)
          (maybe/just (inc n))
          (maybe/nothing)))

      (ctx/with-context maybe/context
        (m/traverse inc-if-even [2 4]))
      ;; => #<Just [3 4]>
  "
  ([f tv]
   (p/-traverse (p/-get-context tv) f tv))
  ([ctx f tv]
   (ctx/with-context ctx
     (p/-traverse (p/-get-context tv) f tv))))
