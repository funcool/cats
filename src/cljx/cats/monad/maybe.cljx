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

(ns cats.monad.maybe
  "The Maybe monad implementation and helpers functions
  for working with maybe related types.

      (require '[cats.monad.maybe :as maybe])

      (maybe/just 1)
      ;; => #<Just [1]>
  "
  (:require [cats.protocols :as proto]))

(declare maybe-monad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructors and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Just [v]
  proto/Context
  (get-context [_] maybe-monad)

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
    (if (instance? Just other)
      (= v (.-v other))
      false))

  #+clj
  (toString [self]
    (with-out-str (print [v])))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [_ other]
    (if (instance? Just other)
      (= v (.-v other))
      false)))

(deftype Nothing []
  proto/Context
  (get-context [_] maybe-monad)

  proto/Extract
  (extract [_] nil)

  #+clj
  clojure.lang.IDeref
  #+clj
  (deref [_] nil)

  #+cljs
  IDeref
  #+cljs
  (-deref [_] nil)

  #+clj
  Object
  #+clj
  (equals [_ other]
    (instance? Nothing other))

  #+clj
  (toString [_]
    (with-out-str (print "")))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [_ other]
    (instance? Nothing other)))

(alter-meta! #'->Nothing assoc :private true)
(alter-meta! #'->Just assoc :private true)

(defn maybe?
  [v]
  (or (instance? Just v)
      (instance? Nothing v)
      (nil? v)))

(defn just
  ([v]
     (Just. v))
  ([]
     (Just. nil)))

(defn nothing
  []
  (Nothing.))

(defn just?
  [v]
  (instance? Just v))

(defn nothing?
  [v]
  (or
   (nil? v)
   (instance? Nothing v)))

(defn from-maybe
  "Return inner value from maybe monad. Accepts an optional default value.

  Let see some examples:

      (from-maybe (just 1))
      ;=> 1
      (from-maybe (just 1) 42)
      ;=> 1
      (from-maybe (nothing))
      ;=> nil
      (from-maybe (nothing) 42)
      ;=> 42
  "
  ([mv]
   {:pre [(maybe? mv)]}
   (when (just? mv)
     (proto/extract mv)))
  ([mv default]
   {:pre [(maybe? mv)]}
   (if (just? mv)
     (.-v mv)
     default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def maybe-monad
  (reify
    proto/Functor
    (fmap [_ f mv]
      (if (nothing? mv)
        mv
        (just (f (from-maybe mv)))))

    proto/Applicative
    (pure [_ v]
      (just v))
    (fapply [m af av]
      (if (nothing? af)
        af
        (proto/fmap m (from-maybe af) av)))

    proto/Monad
    (mreturn [_ v]
      (just v))
    (mbind [_ mv f]
      (if (nothing? mv)
        mv
        (f (from-maybe mv))))

    proto/MonadZero
    (mzero [_]
      (nothing))

    proto/MonadPlus
    (mplus [_ mv mv']
      (if (just? mv)
        mv
        mv'))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad transformer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn maybe-transformer [inner-monad]
  (reify
    proto/Functor
    (fmap [_ f fv]
      (proto/fmap inner-monad
                  #(proto/fmap maybe-monad f %)
                  fv))

    proto/Monad
    (mreturn [m v]
      (proto/mreturn inner-monad (just v)))

    (mbind [_ mv f]
      (proto/mbind inner-monad
                   mv
                   (fn [maybe-v]
                     (if (just? maybe-v)
                       (f (from-maybe maybe-v))
                       (proto/mreturn inner-monad (nothing))))))

    proto/MonadZero
    (mzero [_]
      (proto/mreturn inner-monad (nothing)))

    proto/MonadPlus
    (mplus [_ mv mv']
      (proto/mbind inner-monad
                   mv
                   (fn [maybe-v]
                     (if (just? maybe-v)
                       (proto/mreturn inner-monad maybe-v)
                       mv'))))

    proto/MonadTrans
    (base [_]
      maybe-monad)

    (inner [_]
      inner-monad)

    (lift [_ mv]
      (proto/mbind inner-monad
                   mv
                   (fn [v]
                     (proto/mreturn inner-monad (just v)))))))
