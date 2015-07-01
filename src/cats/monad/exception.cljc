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

(ns cats.monad.exception
  "The Exception monad.

  Also known as Try monad, popularized by Scala.

  It represents a computation that may either result
  in an exception or return a successfully computed
  value. Is very similar to Either monad, but is
  semantically different.

  It consists in two types: Success and Failure. The
  Success type is a simple wrapper like Right of Either
  monad. But the Failure type is slightly different
  from Left, because it is forced to wrap an instance
  of Throwable (or Error in cljs).

  The most common use case of this monad is for wrap
  third party libraries that uses standard Exception
  based error handling. In normal circumstances you
  should use Either instead.

  The types defined for Exception monad (Success and
  Failure) also implementes the clojure IDeref interface
  which facilitates libraries developing using monadic
  composition without forcing a user of that library
  to use or understand monads.

  That is because when you will dereference the
  failure instance, it will reraise the containing
  exception."
  #?(:clj
     (:require [cats.protocols :as proto]
               [cats.core :refer [with-monad]]))

  #?@(:cljs
      [(:require [cats.protocols :as proto])

       (:require-macros [cats.monad.exception :refer [try-on]]
                        [cats.core :refer [with-monad]])]))

(defn throw-exception
  [message]
  (throw (#?(:clj IllegalArgumentException.
             :cljs js/Error.)
            message)))

(defn throwable?
  "Return true if `v` is an instance of
  the Throwable or js/Error type."
  [e]
  (instance? #?(:clj Exception :cljs js/Error) e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types and implementations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare exception-monad)

(deftype Success [v]
  proto/Context
  (get-context [_] exception-monad)

  proto/Extract
  (extract [_] v)

  #?(:clj clojure.lang.IDeref
     :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_] v)

  #?@(:clj
      [Object
       (equals [self other]
         (if (instance? Success other)
           (= v (.-v other))
           false))

       (toString [self]
         (with-out-str (print [v])))])

  #?@(:cljs
       [cljs.core/IEquiv
        (-equiv [_ other]
                (if (instance? Success other)
                  (= v (.-v other))
                  false))]))

(deftype Failure [e]
  proto/Context
  (get-context [_] exception-monad)

  proto/Extract
  (extract [_] e)

  #?(:clj clojure.lang.IDeref
     :cljs IDeref)
  (#?(:clj deref :cljs -deref) [_] (throw e))

  #?@(:clj
      [Object
       (equals [self other]
         (if (instance? Failure other)
           (= e (.-e other))
           false))

       (toString [self]
         (with-out-str (print [e])))])

  #?@(:cljs
       [cljs.core/IEquiv
        (-equiv [_ other]
                (if (instance? Failure other)
                  (= e (.-e other))
                  false))]))

(alter-meta! #'->Success assoc :private true)
(alter-meta! #'->Failure assoc :private true)

(defn success
  "A Success type constructor.

  It wraps any arbitrary value into
  success type."
  [v]
  (Success. v))

(defn failure
  "A failure type constructor.

  If a provided parameter is an exception, it wraps
  it in a `Failure` instance and return it. But if
  a provided parameter is arbitrary data, it tries
  create an exception from it using clojure `ex-info`
  function.

  Take care that `ex-info` function in clojurescript
  differs a little bit from clojure."
  ([e] (failure e ""))
  ([e message]
   (if (throwable? e)
     (Failure. e)
     (Failure. (ex-info message e)))))

(defn success?
  "Return true if `v` is an instance of
  the Success type."
  [v]
  (or (instance? Success v)
      (and (satisfies? proto/Context v)
           (not (instance? Throwable v)))))

(defn failure?
  "Return true if `v` is an instance of
  the Failure type."
  [v]
  (or (instance? Failure v)
      (and (satisfies? proto/Context v)
           (instance? Throwable v))))

(defn exception?
  "Return true in case of `v` is instance
  of Exception monad."
  [v]
  (if (satisfies? proto/Context v)
    (identical? (proto/get-context v) exception-monad)
    false))

(defn extract
  "Return inner value from exception monad.

  This is a specialized version of `cats.core/extract`
  for Exception monad types that allows set up
  the default value.

  If a provided `mv` is an instance of Failure type
  it will re raise the inner exception. If you need
  extract value without raising it, use `cats.core/extract`
  function for it."
  ([mv]
   {:pre [(exception? mv)]}
   (if (success? mv)
     (proto/extract mv)
     (throw (proto/extract mv))))
  ([mv default]
   {:pre [(exception? mv)]}
   (if (success? mv)
     (proto/extract mv)
     default)))

(defn ^{:no-doc true}
  exec-try-on
  [func]
  (try
    (let [result (func)]
      (cond
        (throwable? result) (failure result)
        (exception? result) result
        :else (success result)))
    (catch #?(:clj Exception
              :cljs js/Error) e (failure e))))

(defn ^{:no-doc true}
  exec-try-or-else
  [func defaultvalue]
  (let [result (exec-try-on func)]
    (if (failure? result)
      (success defaultvalue)
      result)))

(defn ^{:no-doc true}
  exec-try-or-recover
  [func recoverfn]
  (let [result (exec-try-on func)]
    (with-monad exception-monad
      (if (failure? result)
        (recoverfn (.-e result))
        result))))

#?(:clj
   (defmacro try-on
    "Wraps a computation and return success of failure."
    [expr]
    `(let [func# (fn [] ~expr)]
       (exec-try-on func#))))

#?(:clj
   (defmacro try-or-else
     [expr defaultvalue]
     `(let [func# (fn [] ~expr)]
        (exec-try-or-else func# ~defaultvalue))))

#?(:clj
   (defmacro try-or-recover
     [expr func]
     `(let [func# (fn [] ~expr)]
        (exec-try-or-recover func# ~func))))

(defn wrap
  "Wrap a function in a try monad.

  Is a high order function that accept a function
  as parameter and returns an other that returns
  success or failure depending of result of the
  first function."
  [func]
  (let [metadata (meta func)]
    (-> (fn [& args] (try-on (apply func args)))
        (with-meta metadata))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  exception-monad
  (reify
    proto/Functor
    (fmap [_ f s]
      (if (success? s)
        (try-on (f (proto/extract s)))
        s))

    proto/Applicative
    (pure [_ v]
      (success v))

    (fapply [m af av]
      (if (success? af)
        (proto/fmap m (proto/extract af) av)
        af))

    proto/Monad
    (mreturn [_ v]
      (success v))

    (mbind [_ s f]
      (if (success? s)
        (f (proto/extract s))
        s))))
