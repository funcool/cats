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

  (:require [cats.protocols :as p]
            [cats.util :as util]
            #?(:clj [cats.context :as ctx]
               :cljs [cats.context :as ctx :include-macros true]))
  #?(:cljs
     (:require-macros [cats.monad.exception :refer (try-on)])))

;; --- Helpers

(defn throw-exception
  [^String message]
  (throw (#?(:clj IllegalArgumentException.
             :cljs js/Error.)
            message)))

(defn throwable?
  "Return true if `v` is an instance of
  the Throwable or js/Error type."
  [e]
  (instance? #?(:clj Exception :cljs js/Error) e))

;; --- Types and implementations.

(declare context)

(defrecord Success [success]
  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] success)

  p/Printable
  (-repr [_]
    (str "#<Success " (pr-str success) ">"))

  #?@(:cljs [cljs.core/IDeref
             (-deref [_] success)]
      :clj  [clojure.lang.IDeref
             (deref [_] success)]))

(defrecord Failure [failure]
  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] failure)

  p/Printable
  (-repr [_]
    (str "#<Failure " (pr-str failure) ">"))

  #?@(:cljs [cljs.core/IDeref
             (-deref [_] (throw failure))]
      :clj  [clojure.lang.IDeref
             (deref [_] (throw failure))]))

(alter-meta! #'->Success assoc :private true)
(alter-meta! #'->Failure assoc :private true)

(util/make-printable Success)
(util/make-printable Failure)

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
  (instance? Success v))

(defn failure?
  "Return true if `v` is an instance of
  the Failure type."
  [v]
  (instance? Failure v))

(defn exception?
  "Return true in case of `v` is instance
  of Exception monad."
  [v]
  (cond
    (or (instance? Failure v)
        (instance? Success v))
    true

    (satisfies? p/Contextual v)
    (identical? (p/-get-context v) context)

    :else false))

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
     (p/-extract mv)
     (throw (p/-extract mv))))
  ([mv default]
   {:pre [(exception? mv)]}
   (if (success? mv)
     (p/-extract mv)
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
    (ctx/with-context context
      (if (failure? result)
        (recoverfn (.-failure ^Failure result))
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

;; --- Monad definition

(def ^{:no-doc true}
  context
  (reify
    p/Context
    p/Functor
    (-fmap [_ f s]
      (if (success? s)
        (try-on (f (p/-extract s)))
        s))

    p/Applicative
    (-pure [_ v]
      (success v))

    (-fapply [m af av]
      (if (success? af)
        (p/-fmap m (p/-extract af) av)
        af))

    p/Monad
    (-mreturn [_ v]
      (success v))

    (-mbind [_ s f]
      (assert (exception? s) (str "Context mismatch: " (p/-repr s)
                                  " is not allowed to use with exception context."))

      (if (success? s)
        (f (p/-extract s))
        s))

    p/Printable
    (-repr [_]
      "#<Exception>")))

(util/make-printable (type context))
