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

(ns cats.labs.sugar
  "Experimental syntax sugar for core abstractions. Sugar that
   proves its worthiness will graduate to the cats.core namespace."
  (:require [cats.core :as m]))

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
    `(m/alet [~@(interleave syms args)]
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
    `(m/mlet [~g ~expr
              ~@(interleave (repeat g) (map pstep forms))]
           (m/return ~g))))

(defmacro ->>=
  "Like ->>, but with monadic binding instead of pure application.
   See `cats.labs.sugar/->=` for more in-depth discussion."
  [expr & forms]
  (let [g (gensym)
        pstep (fn [step] `(->> ~g ~step))]
    `(m/mlet [~g ~expr
              ~@(interleave (repeat g) (map pstep forms))]
           (m/return ~g))))

(defmacro as->=
  "Like `as->`, but with monadic binding instead of pure application.
   See `cats.labs.sugar/->=` for more in-depth discussion."
  [expr name & forms]
  `(m/mlet [~name ~expr
            ~@(interleave (repeat name) forms)]
     (m/return ~name)))
