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

(ns cats.monad.maybe
  "The Maybe monad implementation and helpers functions
  for working with maybe related types.

      (require '[cats.monad.maybe :as maybe])

      (maybe/just 1)
      ;; => #<Just [1]>
  "
  (:require [cats.protocols :as p]
            [cats.context :as ctx]))

(declare context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructors and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Just [v]
  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] v)

  #?@(:cljs [cljs.core/IDeref
             (-deref [_] v)]
      :clj  [clojure.lang.IDeref
             (deref [_] v)])

  #?@(:clj
      [Object
       (equals [self other]
         (if (instance? Just other)
           (= v (.-v other))
           false))

       (toString [self]
         (with-out-str (print [v])))])

  #?@(:cljs
      [cljs.core/IEquiv
       (-equiv [_ other]
         (if (instance? Just other)
           (= v (.-v other))
           false))]))

(deftype Nothing []
  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] nil)

  #?@(:cljs [cljs.core/IDeref
             (-deref [_] nil)]
      :clj  [clojure.lang.IDeref
             (deref [_] nil)])

  #?@(:clj
      [Object
       (equals [self other]
         (instance? Nothing other))

       (toString [self]
         (with-out-str (print "")))])

  #?@(:cljs
      [cljs.core/IEquiv
       (-equiv [_ other]
         (instance? Nothing other))]))

(alter-meta! #'->Nothing assoc :private true)
(alter-meta! #'->Just assoc :private true)

(defn maybe?
  "Return true in case of `v` is instance
  of Maybe monad."
  [v]
  (if (satisfies? p/Contextual v)
    (identical? (p/-get-context v) context)
    false))

(defn just
  "A Just type constructor.

  Without arguments it returns a Just instance
  with nil as wrapped value."
  ([] (Just. nil))
  ([v] (Just. v)))

(defn nothing
  "A Nothing type constructor."
  []
  (Nothing.))

(defn just?
  "Returns true if `v` is an instance
  of `Just` type."
  [v]
  (instance? Just v))

(defn nothing?
  "Returns true if `v` is an instance
  of `Nothing` type or is nil."
  [v]
  (or
   (nil? v)
   (instance? Nothing v)))

(defn from-maybe
  "Return inner value from maybe monad.

  This is a specialized version of `cats.core/extract`
  for Maybe monad types that allows set up
  the default value.

  Let see some examples:

      (from-maybe (just 1))
      ;=> 1

      (from-maybe (nothing))
      ;=> nil

      (from-maybe (nothing) 42)
      ;=> 42
  "
  ([mv]
   {:pre [(maybe? mv)]}
   (when (just? mv)
     (p/-extract mv)))
  ([mv default]
   {:pre [(maybe? mv)]}
   (if (just? mv)
     (p/-extract mv)
     default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  context
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Semigroup
    (-mappend [ctx mv mv']
      (cond
        (nothing? mv) mv'
        (nothing? mv') mv
        :else (just (let [mv (p/-extract mv)
                          mv' (p/-extract mv')]
                      (p/-mappend (p/-get-context mv) mv mv')))))

    p/Monoid
    (-mempty [_]
      (nothing))

    p/Functor
    (-fmap [_ f mv]
      (if (nothing? mv)
        mv
        (just (f (p/-extract mv)))))

    p/Applicative
    (-pure [_ v]
      (just v))
    (-fapply [m af av]
      (if (nothing? af)
        af
        (p/-fmap m (p/-extract af) av)))

    p/Monad
    (-mreturn [_ v]
      (just v))
    (-mbind [_ mv f]
      (if (nothing? mv)
        mv
        (f (p/-extract mv))))

    p/MonadZero
    (-mzero [_]
      (nothing))

    p/MonadPlus
    (-mplus [_ mv mv']
      (if (just? mv)
        mv
        mv'))

    p/Foldable
    (-foldl [_ f z mv]
      (if (just? mv)
        (f z (p/-extract mv))
        z))

    (-foldr [_ f z mv]
      (if (just? mv)
        (f (p/-extract mv) z)
        z))

    p/Traversable
    (-traverse [_ f mv]
      (if (just? mv)
        (let [a (f (p/-extract mv))]
          (p/-fmap (p/-get-context a) just a))
        (p/-pure (ctx/get-current) mv)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad Transformer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn maybe-t
  "The maybe transformer constructor."
  [inner]
  (reify
    p/Context
    (-get-level [_] ctx/+level-transformer+)

    p/Functor
    (-fmap [_ f fv]
      (p/-fmap inner
               #(p/-fmap context f %)
               fv))

    p/Monad
    (-mreturn [m v]
      (p/-mreturn inner (just v)))

    (-mbind [_ mv f]
      (p/-mbind inner
                mv
                (fn [maybe-v]
                  (if (just? maybe-v)
                    (f (p/-extract maybe-v))
                    (p/-mreturn inner (nothing))))))

    p/MonadZero
    (-mzero [_]
      (p/-mreturn inner (nothing)))

    p/MonadPlus
    (-mplus [_ mv mv']
      (p/-mbind inner
                mv
                (fn [maybe-v]
                  (if (just? maybe-v)
                    (p/-mreturn inner maybe-v)
                    mv'))))

    p/MonadTrans
    (-lift [_ mv]
      (p/-mbind inner
                mv
                (fn [v]
                  (p/-mreturn inner (just v)))))))

(def ^{:doc "Deprecated alias for `maybe-t`."
       :deprecated true}
  maybe-transformer maybe-t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn maybe
  "Given a default value, a maybe and a function, return the default
  if the maybe is a nothing; if its a just, apply the function to the
  value it contains and return the result."
  [default m f]
  {:pre [(maybe? m)]}
  (if (nothing? m)
    default
    (f (p/-extract m))))

(defn seq->maybe
  "Given a collection, return a nothing if its empty or a just with its
  first element if its not."
  [coll]
  (if (empty? coll)
    (nothing)
    (just (first coll))))

(defn maybe->seq
  "Given a maybe, return an empty seq if its nothing or a one-element seq
  with its value if its not."
  [m]
  {:pre [(maybe? m)]}
  (if (nothing? m)
    (lazy-seq [])
    (lazy-seq [(p/-extract m)])))

(def ^{:private true :no-doc true}
  +extract-just-xform+
  (comp
   (filter just?)
   (map p/-extract)))

(defn cat-maybes
  "Given a collection of maybes, return a sequence of the values
  that the just's contain."
  [coll]
  (sequence +extract-just-xform+ coll))

(defn map-maybe
  "Given a maybe-returning function and a collection, map the function over
  the collection returning the values contained in the just values of the
  resulting collection."
  [mf coll]
  (cat-maybes (map mf coll)))
