;; Copyright (c) 2014-2015 Andrey Antukh <niwi@niwi.be>
;; Copyright (c) 2014-2015 Alejandro Gómez
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

(declare either-monad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructor and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Right [v]
  proto/Context
  (get-context [_] either-monad)

  proto/Extract
  (extract [_] v)

  #+clj
  clojure.lang.IDeref
  #+clj
  (deref [_] v)

  #+cljs
  IDeref
  #+cljs
  (-deref [_] v)

  #+clj
  Object
  #+clj
  (equals [self other]
    (if (instance? Right other)
      (= v (.-v other))
      false))

  #+clj
  (toString [self]
    (with-out-str (print [v])))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [self other]
    (if (instance? Right other)
      (= v (.-v other))
      false)))

(deftype Left [v]
  proto/Context
  (get-context [_] either-monad)

  proto/Extract
  (extract [_] v)

  #+clj
  clojure.lang.IDeref
  #+clj
  (deref [_] v)

  #+cljs
  IDeref
  #+cljs
  (-deref [_] v)

  #+clj
  Object
  #+clj
  (equals [self other]
    (if (instance? Left other)
      (= v (.-v other))
      false))

  #+clj
  (toString [self]
    (with-out-str (print [v])))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [self other]
    (if (instance? Left other)
      (= v (.-v other))
      false)))

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

(defn ^{:deprecated true}
  from-either
  "Return inner value of either monad.

  This is a specialized version of `cats.core/extract`
  for Either monad.

  The use of this function is DEPRECATED and
  `cats.core/extract` should be used."
  [mv]
  (proto/extract mv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def either-monad
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

(defn either-transformer [inner-monad]
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
                       (f (from-either either-v))))))

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
