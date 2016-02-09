;; Copyright (c) 2014-2015 Andrey Antukh <niwi@niwi.nz>
;; Copyright (c) 2014-2015 Alejandro Gómez <alejandro@dialelo.com>
;; All rights reserved
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

(ns cats.builtin
  "Clojure(Script) built-in types extensions."
  (:require [clojure.set :as s]
            [cats.monad.maybe :as maybe]
            [cats.protocols :as p]
            [cats.context :as ctx]
            [cats.core :as m]
            [cats.util :as util]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nil as Nothing of Maybe monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type nil
  p/Contextual
  (-get-context [_] maybe/context)

  p/Extract
  (-extract [_] nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sequence Monad i.e. PersistentList
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sequence-context
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (into sv' (reverse sv)))

    p/Monoid
    (-mempty [_] ())

    p/Functor
    (-fmap [_ f v]
      (loop [[h & t :as c] v
             result ()]
        (if (empty? c)
          (reverse result)
          (recur t (cons (f h) result)))))

    p/Applicative
    (-pure [_ v] (list v))

    (-fapply [_ self av]
      ;; Each function (outer loop) applied to each value (inner loop).
      (->> (loop [[h & t :as c] self
                   result ()]
             (if (empty? c)
               result
               (recur t
                      (cons (loop [[h' & t' :as c'] av
                               result' ()]
                              (if (empty? c')
                                result'
                                (recur t' (cons (h h') result'))))
                            result))))
           ;; Note that both `result` & `result'` above are
           ;; in reverse order.
           ;; Conjing elements of %2 into %1 below is done in
           ;; in reverse order, so final result is correctly
           ;; ordered.
           (reduce #(into %1 %2) ())))


    p/Monad
    (-mreturn [_ v]
      (list v))

    (-mbind [_ self f]
      (->> (loop [[h & t :as c] self
                  result ()]
             (if (empty? c)
               result
               (recur t (cons (f h) result))))
           ;; Note that `result` above is in reverse order.
           ;; Conjing elements of %2 into %1 below is done in
           ;; in reverse order, so final result is correctly
           ;; ordered.
           (reduce #(into %1 %2) ())))

    p/MonadZero
    (-mzero [_] ())

    p/MonadPlus
    (-mplus [_ mv mv']
      (into mv' (reverse mv)))

    p/Foldable
    (-foldr [ctx f z xs]
      (let [x (first xs)]
        (if (nil? x)
          z
          (let [xs (rest xs)]
            (f x (p/-foldr ctx f z xs))))))

    (-foldl [ctx f z xs]
      (reduce f z xs))

    p/Traversable
    (-traverse [ctx f tv]
      (let [as (p/-fmap ctx f tv)]
        (p/-foldr ctx
                  (fn [a acc]
                    (m/alet [x a
                             xs acc]
                      (cons x xs)))
                  (m/pure ())
                  as)))

    p/Printable
    (-repr [_]
      "#<List>")))

(util/make-printable (type sequence-context))

(extend-type #?(:clj  clojure.lang.PersistentList
                :cljs cljs.core.List)
  p/Contextual
  (-get-context [_] sequence-context))

(extend-type #?(:clj  clojure.lang.PersistentList$EmptyList
                :cljs cljs.core.EmptyList)
  p/Contextual
  (-get-context [_] sequence-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lazy Sequence Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def lazy-sequence-context
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (concat sv sv'))

    p/Monoid
    (-mempty [_]
      (lazy-seq []))

    p/Functor
    (-fmap [_ f v]
      (map f v))

    p/Applicative
    (-pure [_ v]
      (lazy-seq [v]))

    (-fapply [_ self av]
      (for [f self
            v av]
           (f v)))

    p/Monad
    (-mreturn [_ v]
      (lazy-seq [v]))

    (-mbind [_ self f]
      (apply concat (map f self)))

    p/MonadZero
    (-mzero [_]
      (lazy-seq []))

    p/MonadPlus
    (-mplus [_ mv mv']
      (concat mv mv'))

    p/Foldable
    (-foldr [ctx f z xs]
      (let [x (first xs)]
        (if (nil? x)
          z
          (let [xs (rest xs)]
            (f x (p/-foldr ctx f z xs))))))

    (-foldl [ctx f z xs]
      (reduce f z xs))

    p/Traversable
    (-traverse [ctx f tv]
      (let [as (p/-fmap ctx f tv)]
        (p/-foldr ctx
                  (fn [a acc]
                    (m/alet [x a
                             xs acc]
                      (cons x xs)))
                  (m/pure (lazy-seq []))
                  as)))

    p/Printable
    (-repr [_]
      "#<LazySequence>")))

(util/make-printable (type lazy-sequence-context))

(extend-type #?(:clj  clojure.lang.LazySeq
                :cljs cljs.core.LazySeq)
  p/Contextual
  (-get-context [_] lazy-sequence-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Range
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def range-context
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Foldable
    (-foldr [ctx f z xs]
      (let [x (first xs)]
        (if (nil? x)
          z
          (let [xs (rest xs)]
            (f x (p/-foldr ctx f z xs))))))

    (-foldl [ctx f z xs]
      (reduce f z xs))

    p/Printable
    (-repr [_]
      "#<Range>")))

(util/make-printable (type range-context))

(extend-type #?(:clj  clojure.lang.LongRange
                :cljs cljs.core.Range)
  p/Contextual
  (-get-context [_] range-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def vector-context
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (into sv sv'))

    p/Monoid
    (-mempty [_]
      [])

    p/Functor
    (-fmap [_ f v]
      (vec (map f v)))

    p/Applicative
    (-pure [_ v]
      [v])

    (-fapply [_ self av]
      (vec (for [f self
                 v av]
             (f v))))

    p/Monad
    (-mreturn [_ v]
      [v])

    (-mbind [_ self f]
      (vec (mapcat f self)))

    p/MonadZero
    (-mzero [_]
      [])

    p/MonadPlus
    (-mplus [_ mv mv']
      (into mv mv'))

    p/Foldable
    (-foldr [ctx f z xs]
      (letfn [(rf [acc v] (f v acc))]
        (reduce rf z (reverse xs))))

    (-foldl [ctx f z xs]
      (reduce f z xs))

    p/Traversable
    (-traverse [ctx f tv]
      (let [as (p/-fmap ctx f tv)]
        (p/-foldl ctx
                  (fn [acc a]
                    (m/alet [x a
                             xs acc]
                       (conj xs x)))
                  (m/pure [])
                  as)))

    p/Printable
    (-repr [_]
      "#<Vector>")))

(util/make-printable (type vector-context))

(extend-type #?(:clj clojure.lang.PersistentVector
                :cljs cljs.core.PersistentVector)
  p/Contextual
  (-get-context [_] vector-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Map Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def map-context
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (merge sv sv'))

    p/Monoid
    (-mempty [_]
      {})

    p/Functor
    (-fmap [_ f v]
      (into {} (map (fn [[key value]] [key (f value)]) v)))

    p/Foldable
    (-foldr [ctx f z xs]
      (letfn [(rf [acc v] (f v acc))]
        (reduce rf z xs)))

    (-foldl [ctx f z xs]
      (reduce f z xs))

    p/Printable
    (-repr [_]
      "#<Map>")))

(util/make-printable (type map-context))

(extend-type #?(:clj clojure.lang.PersistentArrayMap
                :cljs cljs.core.PersistentArrayMap)
  p/Contextual
  (-get-context [_] map-context))

(extend-type #?(:clj clojure.lang.PersistentHashMap
                :cljs cljs.core.PersistentHashMap)
  p/Contextual
  (-get-context [_] map-context))

(extend-type #?(:clj clojure.lang.PersistentTreeMap
                :cljs cljs.core.PersistentTreeMap)
  p/Contextual
  (-get-context [_] map-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def set-context
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (s/union sv (set sv')))

    p/Monoid
    (-mempty [_]
      #{})

    p/Functor
    (-fmap [_ f self]
      (set (map f self)))

    p/Applicative
    (-pure [_ v]
      #{v})

    (-fapply [_ self av]
      (set (for [f self
                 v av]
             (f v))))

    p/Monad
    (-mreturn [_ v]
      #{v})

    (-mbind [_ self f]
      (apply s/union (map f self)))

    p/MonadZero
    (-mzero [_]
      #{})

    p/MonadPlus
    (-mplus [_ mv mv']
      (s/union mv mv'))

    p/Printable
    (-repr [_]
      "#<Set>")))

(util/make-printable (type set-context))

(extend-type #?(:clj clojure.lang.PersistentHashSet
                :cljs cljs.core.PersistentHashSet)
  p/Contextual
  (-get-context [_] set-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def function-context
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [ctx f g] (comp f g))

    p/Monoid
    (-mempty [_] identity)

    p/Functor
    (-fmap [_ f self]
      (comp f self))

    p/Applicative
    (-pure [_ v]
      (constantly v))

    (-fapply [_ self av]
      (fn [x] ((self x) (av x))))

    p/Monad
    (-mreturn [_ v]
      (constantly v))

    (-mbind [_ self f]
      (fn [r] ((f (self r)) r)))

    p/Printable
    (-repr [_]
      "#<Function>")))

(util/make-printable (type function-context))

(extend-type #?(:clj clojure.lang.IFn
                :cljs cljs.core.IFn)
  p/Contextual
  (-get-context [_] function-context))

#?(:cljs
   (extend-type function
     p/Contextual
     (-get-context [_] function-context)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monoids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def any-monoid
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (or sv sv'))

    p/Monoid
    (-mempty [_]
      false)

    p/Printable
    (-repr [_]
      "#<Any>")))

(util/make-printable (type any-monoid))

(def all-monoid
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (and sv sv'))

    p/Monoid
    (-mempty [_]
      true)

    p/Printable
    (-repr [_]
      "#<All>")))

(util/make-printable (type all-monoid))

(def sum-monoid
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (+ sv sv'))

    p/Monoid
    (-mempty [_]
      0)

    p/Printable
    (-repr [_]
      "#<Sum>")))

(util/make-printable (type sum-monoid))

(def prod-monoid
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (* sv sv'))

    p/Monoid
    (-mempty [_]
      1)

    p/Printable
    (-repr [_]
      "#<Product>")))

(util/make-printable (type prod-monoid))

(def string-monoid
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (str sv sv'))

    p/Monoid
    (-mempty [_]
      "")

    p/Printable
    (-repr [_]
      "#<String>")))

(util/make-printable (type string-monoid))

(extend-type #?(:clj java.lang.String
                :cljs string)
  p/Contextual
  (-get-context [_] string-monoid))
