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
;; THEORY OF LIABILITtY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVE N IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(ns cats.applicative.validation
  "The Validation applicative implementation and helper functions
  for validating values. Isomorphic to Either."
  (:require [cats.protocols :as p]
            [cats.monad.either :as either]))

(declare context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructor and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Ok [v]
  p/Context
  (-get-context [_] context)

  p/Extract
  (-extract [_] v)

  #?(:clj clojure.lang.IDeref
     :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_] v)

  #?@(:clj
      [Object
       (equals [self other]
         (if (instance? Ok other)
           (= v (.-v other))
           false))

       (toString [self]
         (with-out-str (print [v])))])

  #?@(:cljs
      [cljs.core/IEquiv
       (-equiv [_ other]
         (if (instance? Ok other)
           (= v (.-v other))
           false))]))

(deftype Fail [v]
  p/Context
  (-get-context [_] context)

  p/Extract
  (-extract [_] v)

  #?(:clj clojure.lang.IDeref
     :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_] v)

  #?@(:clj
      [Object
       (equals [self other]
         (if (instance? Fail other)
           (= v (.-v other))
           false))

       (toString [self]
         (with-out-str (print [v])))])

  #?@(:cljs
      [cljs.core/IEquiv
       (-equiv [_ other]
         (if (instance? Fail other)
           (= v (.-v other))
           false))]))

(alter-meta! #'->Ok assoc :private true)
(alter-meta! #'->Fail assoc :private true)

(defn ok
  "An Ok type constructor."
  [v]
  (Ok. v))

(defn fail
  "A Fail type constructor."
  ([]
   (Fail. []))
  ([v]
   (Fail. v)))

(defn ok?
  "Return true if `v` is an instance
  of Ok type."
  [v]
  (instance? Ok v))

(defn fail?
  "Return true if `v` is an instance
  of Fail type."
  [v]
  (instance? Fail v))

(defn validation?
  "Return true in case of `v` is instance
  of the Validation applicative."
  [v]
  (if (satisfies? p/Context v)
    (identical? (p/-get-context v) context)
    false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Applicative definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  context
  (reify
    p/ContextClass
    (-get-level [_] 10)

    p/Semigroup
    (-mappend [_ sv sv']
      (cond
        (and (fail? sv) (fail? sv'))
        (fail (let [sv (p/-extract sv)
                    sv' (p/-extract sv')]
                (p/-mappend (p/-get-context sv) sv sv')))

        (ok? sv) sv
        :else sv'))

    p/Monoid
    (-mempty [_]
      (fail))

    p/Functor
    (-fmap [_ f s]
      (if (ok? s)
        (ok (f (p/-extract s)))
        s))

    p/Foldable
    (-foldl [_ f z mv]
      (if (ok? mv)
        (f z (p/-extract mv))
        z))

    (-foldr [_ f z mv]
      (if (ok? mv)
        (f (p/-extract mv) z)
        z))

    p/Applicative
    (-pure [_ v]
      (ok v))

    (-fapply [_ af av]
      (cond
        (and (ok? af) (ok? av))
        (ok ((p/-extract af) (p/-extract av)))

        (and (fail? af) (fail? av))
        (fail (let [af (p/-extract af)
                    av (p/-extract av)]
                (p/-mappend (p/-get-context af) af av)))

        (ok? af) av
        :else af))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Either isomorphism
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn validation->either
  [av]
  {:pre [(validation? av)]}
  (if (ok? av)
    (either/right (.-v av))
    (either/left (.-v av))))

(defn either->validation
  [ae]
  {:pre [(either/either? ae)]}
  (if (either/right? ae)
    (ok (.-v ae))
    (fail (.-v ae))))
