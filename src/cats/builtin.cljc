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

(ns cats.builtin
  "Clojure(Script) built-in types extensions."
  (:require [clojure.set :as s]
            [cats.monad.maybe :as maybe]
            [cats.protocols :as p]
            [cats.context :as ctx]
            [cats.data :as d]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nil as Nothing of Maybe monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type nil
  p/Context
  (-get-context [_] maybe/context)

  p/Extract
  (-extract [_] nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (Lazy) Sequence Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def sequence-context
  (reify
    p/ContextClass
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
      (reduce f z xs))))

(extend-type #?(:clj  clojure.lang.LazySeq
                :cljs cljs.core.LazySeq)
  p/Context
  (-get-context [_] sequence-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Range
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def range-context
  (reify
    p/ContextClass
    (-get-level [_] ctx/+level-default+)

    p/Foldable
    (-foldr [ctx f z xs]
      (let [x (first xs)]
        (if (nil? x)
          z
          (let [xs (rest xs)]
            (f x (p/-foldr ctx f z xs))))))

    (-foldl [ctx f z xs]
      (reduce f z xs))))

(extend-type #?(:clj  clojure.lang.LongRange
                :cljs cljs.core.Range)
  p/Context
  (-get-context [_] range-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vector Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def vector-context
  (reify
    p/ContextClass
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
      (reduce f z xs))))

(extend-type #?(:clj clojure.lang.PersistentVector
                :cljs cljs.core.PersistentVector)
  p/Context
  (-get-context [_] vector-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def set-context
  (reify
    p/ContextClass
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
      (s/union mv mv'))))

(extend-type #?(:clj clojure.lang.PersistentHashSet
                :cljs cljs.core.PersistentHashSet)
  p/Context
  (-get-context [_] set-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monoids
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def map-monoid
  (reify
    p/ContextClass
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (merge sv sv'))

    p/Monoid
    (-mempty [_]
      {})))

(extend-type #?(:clj clojure.lang.PersistentHashMap
                :cljs cljs.core.PersistentHashMap)
  p/Context
  (-get-context [_] map-monoid))

#?(:clj
   (extend-type clojure.lang.PersistentArrayMap
     p/Context
     (-get-context [_] map-monoid))
   :cljs
   (extend-type cljs.core.PersistentArrayMap
     p/Context
     (-get-context [_] map-monoid)))

#?(:clj
   (extend-type clojure.lang.PersistentTreeMap
     p/Context
     (-get-context [_] map-monoid))
   :cljs
   (extend-type cljs.core.PersistentTreeMap
     p/Context
     (-get-context [_] map-monoid)))

(def any-monoid
  (reify
    p/ContextClass
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (or sv sv'))

    p/Monoid
    (-mempty [_]
      false)))

(def all-monoid
  (reify
    p/ContextClass
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (and sv sv'))

    p/Monoid
    (-mempty [_]
      true)))

(def sum-monoid
  (reify
    p/ContextClass
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (+ sv sv'))

    p/Monoid
    (-mempty [_]
      0)))

(def prod-monoid
  (reify
    p/ContextClass
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (* sv sv'))

    p/Monoid
    (-mempty [_]
      1)))

(def string-monoid
  (reify
    p/ContextClass
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [_ sv sv']
      (str sv sv'))

    p/Monoid
    (-mempty [_]
      "")))

(extend-type #?(:clj java.lang.String
                :cljs js/String)
  p/Context
  (-get-context [_] string-monoid))

(defn pair-monoid
  "A pair monoid type constructor."
  [inner-monoid]
  (reify
    p/ContextClass
    (-get-level [_]
      (+ (p/-get-level inner-monoid)
         ctx/+level-default+))

    p/Semigroup
    (-mappend [_ sv sv']
      (d/pair
       (p/-mappend inner-monoid (.fst sv) (.fst sv'))
       (p/-mappend inner-monoid (.snd sv) (.snd sv'))))

    p/Monoid
    (-mempty [_]
      (d/pair
       (p/-mempty inner-monoid)
       (p/-mempty inner-monoid)))))

(extend-type cats.data.Pair
  p/Context
  (-get-context [data]
    (let [first' (.fst data)]
      (pair-monoid (p/-get-context first')))))
