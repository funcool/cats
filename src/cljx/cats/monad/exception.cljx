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

(ns cats.monad.exception
  "The Try Monad.

  The Try type represents a computation that may either result in an exception,
  or return a successfully computed value. It's similar to, but semantically
  different from the Either type."
  #+clj
  (:require [cats.protocols :as proto]
            [cats.core :refer [with-monad]])

  #+cljs
  (:require [cats.protocols :as proto])

  #+cljs
  (:require-macros [cats.monad.exception :refer [try-on]]
                   [cats.core :refer [with-monad]]))

(defn throw-exception
  [message]
  #+clj (throw (IllegalArgumentException. message))
  #+cljs (throw (js/Error. message)))

(defn exception?
  "Check if provided parameter is an instance
  of exception or not."
  [e]
  (instance? #+clj Exception #+cljs js/Error e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types and implementations.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare exception-monad)

(deftype Success [v]
  proto/Context
  (get-context [_] exception-monad)

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
    (if (instance? Success other)
      (= v (.-v other))
      false))

  #+clj
  (toString [self]
    (with-out-str (print [v])))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [self other]
    (if (instance? Success other)
      (= v (.-v other))
      false)))

(deftype Failure [e]
  proto/Context
  (get-context [_] exception-monad)

  proto/Extract
  (extract [_] e)

  #+clj
  clojure.lang.IDeref
  #+clj
  (deref [_] (throw e))

  #+cljs
  IDeref
  #+cljs
  (-deref [_] (throw e))

  Object
  #+clj
  (equals [self other]
    (if (instance? Failure other)
      (= e (.-e other))
      false))

  (toString [_]
    (with-out-str
      (print [e])))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [self other]
    (if (instance? Failure other)
      (= e (.-e other))
      false)))

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

  If a provided parameter is an exceptio, it wraps
  it in a `Failure` instance and return it. But if
  a provided parameter is arbitrary data, it tries
  create an exception from it using clojure `ex-info`
  function.

  Take care that `ex-info` function in clojurescript
  differs a little bit from clojure."
  ([e] (failure e ""))
  ([e message]
   (if (exception? e)
     (Failure. e)
     (Failure. (ex-info message e)))))

(defn success?
  "Check if a provided parameter is a success instance"
  [v]
  (instance? Success v))

(defn failure?
  "Check if a provided parameter is a failure instance."
  [v]
  (instance? Failure v))

(defn try?
  "Check if a provided parameter is instance
  of Try monad."
  [v]
  (let [m (proto/get-context v)]
    (= m exception-monad)))

(defn exec-try-on
  [func]
  (try
    (let [result (func)]
      (if (exception? result)
        (failure result)
        (success result)))
    #+clj
    (catch Throwable e (failure e))
    #+cljs
    (catch js/Error e (failure e))))

(defn exec-try-or-else
  [func defaultvalue]
  (let [result (exec-try-on func)]
    (if (failure? result)
      (success defaultvalue)
      result)))

(defn exec-try-or-recover
  [func recoverfn]
  (let [result (exec-try-on func)]
    (with-monad exception-monad
      (if (failure? result)
        (recoverfn (.-e result))
        result))))

#+clj
(defmacro try-on
  "Wraps a computation and return success of failure."
  [expr]
  `(let [func# (fn [] ~expr)]
     (exec-try-on func#)))

#+clj
(defmacro try-or-else
  [expr defaultvalue]
  `(let [func# (fn [] ~expr)]
     (exec-try-or-else func# ~defaultvalue)))

#+clj
(defmacro try-or-recover
  [expr func]
  `(let [func# (fn [] ~expr)]
     (exec-try-or-recover func# ~func)))

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

(def ^{:doc "The exception monad type definition."}
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
        (proto/fmap m (proto/get-value af) av)
        af))

    proto/Monad
    (mreturn [_ v]
      (success v))

    (mbind [_ s f]
      (if (success? s)
        (f (proto/extract s))
        s))))
