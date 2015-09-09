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

(ns cats.labs.writer
  "The Writer Monad."
  #?(:cljs (:require [cats.protocols :as p]
                     [cats.builtin :as b]
                     [cats.context :as ctx :include-macros true]
                     [cats.data :as d])
     :clj  (:require [cats.protocols :as p]
                     [cats.builtin :as b]
                     [cats.context :as ctx]
                     [cats.data :as d])))

(declare context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol declaration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol MonadWriter
  "A specific case of Monad abstraction that
  allows emulate write operations in pure functional
  way.

  A great example is writing a log message."
  (-listen [m mv] "Given a writer, yield a (value, log) pair as a value.")
  (-tell [m v] "Add the given value to the log.")
  (-pass [m mv]
    "Given a writer whose value is a pair with a function as its second element,
     yield a writer which has the first element of the pair as the value and
     the result of applying the aforementioned function to the log as the new log."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructors and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Writer [mfn]
  p/Contextual
  (-get-context [_] context)

  #?@(:cljs [cljs.core/IFn
             (-invoke [self seed]
               (mfn seed))]
      :clj  [clojure.lang.IFn
             (invoke [self seed]
               (mfn seed))]))

(alter-meta! #'->Writer assoc :private true)

(defn writer
  "The Writer type constructor.

  The purpose of Writer type is wrap a simple
  function that fullfill the writer signature.

  It exists just for avoid extend the clojure
  function type because is very generic type."
  [f]
  (Writer. f))

(defn writer?
  "Return true if `s` is instance
  of Writer type."
  [s]
  (instance? Writer s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  context
  (reify
    p/Context
    (-get-level [_] ctx/+level-default+)

    p/Monad
    (-mreturn [_ v]
      (d/pair v (p/-mempty b/vector-context)))

    (-mbind [_ mv f]
      (let [[v log] mv
            [v' log'] (f v)]
        (d/pair v' (p/-mappend (p/-get-context log) log log'))))

    MonadWriter
    (-tell [_ v]
      (d/pair nil [v]))

    (-listen [_ mv]
      (d/pair mv (second mv)))

    (-pass [_ mv]
      (let [[v f] (first mv)]
        (d/pair v (f (second mv)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad Transformer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn writer-t
  "The Writer transformer constructor."
  [inner-context]
  (reify
    p/Context
    (-get-level [_] ctx/+level-transformer+)

    p/Monad
    (-mreturn [_ v]
      (p/-mreturn inner-context
                  (d/pair v (p/-mempty b/vector-context))))

    (-mbind [_ mv f]
      (p/-mbind inner-context
                mv
                (fn [[v log]]
                  (p/-mbind inner-context
                            (f v)
                            (fn [[v' log']]
                              (p/-mreturn inner-context
                                          (d/pair v' (p/-mappend (p/-get-context log) log log'))))))))

    MonadWriter
    (-tell [_ v]
      (p/-mreturn inner-context (d/pair nil [v])))

    (-listen [_ mv]
      (p/-mbind inner-context
                mv
                (fn [mv]
                  (p/-mreturn inner-context
                              (d/pair mv (second mv))))))

    (-pass [_ mv]
      (p/-mbind inner-context
                mv
                (fn [w]
                  (let [[v f] (first w)
                        log   (second w)]
                    (p/-mreturn inner-context
                                (d/pair v (f log)))))))

    p/MonadTrans
    (-lift [_ mv]
      (p/-mbind inner-context
                mv
                (fn [v]
                  (p/-mreturn inner-context
                              (d/pair v (p/-mempty b/vector-context))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writer monad functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tell
  "Add the value to the log."
  [v]
  (-tell (ctx/get-current context) v))

(defn listen
  "Get the value from the log."
  [mv]
  (-listen (ctx/get-current context) mv))

(defn pass
  "Apply a function to the log."
  [mv]
  (-pass (ctx/get-current context) mv))

(def value first)
(def log second)
