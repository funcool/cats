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

(ns cats.labs.reader
  "The Reader Monad."
  #?(:clj  (:require [cats.context :as ctx]
                     [cats.protocols :as p])
     :cljs (:require [cats.context :as ctx :include-macros true]
                     [cats.protocols :as p])))

(declare context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol declaration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol MonadReader
  "A specific case of Monad abstraction that
  allows a read only access to an environment."
  (-ask [m] "Return the current environment.")
  (-local [m f reader] "Create a reader in a modified version of the environment."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructors and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Reader [mfn]
  p/Context
  (-get-context [_] context)

  #?@(:cljs [cljs.core/IFn
             (-invoke [self seed]
               (mfn seed))]
      :clj  [clojure.lang.IFn
             (invoke [self seed]
               (mfn seed))]))

(alter-meta! #'->Reader assoc :private true)

(defn reader
  "The Reader type constructor.

  The purpose of Reader type is wrap a simple
  function that fullfill the reader signature.

  It exists just for avoid extend the clojure
  function type because is very generic type."
  [f]
  (Reader. f))

(defn reader?
  "Return true if `s` is instance
  of Reader type."
  [s]
  (instance? Reader s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  context
  (reify
    p/ContextClass
    (-get-level [_] 10)

    p/Functor
    (-fmap [_ f fv]
      (reader (fn [env]
                (f (fv env)))))

    p/Monad
    (-mreturn [_ v]
      (reader (fn [env]
                v)))

    (-mbind [_ mv f]
      (reader (fn [env]
                ((f (mv env)) env))))

    MonadReader
    (-ask [_]
      (reader (fn [env]
                env)))

    (-local [_ f mr]
      (reader (fn [env]
                (mr (f env)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad transformer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reader-transformer
  "The Reader transformer constructor."
  [inner-monad]
  (reify
    p/ContextClass
    (-get-level [_] 100)

    p/Functor
    (-fmap [_ f fv]
      (reader (fn [env]
                (p/-fmap inner-monad f (fv env)))))

    p/Monad
    (-mreturn [_ v]
      (p/-mreturn context (p/-mreturn inner-monad v)))

    (-mbind [_ mr f]
      (fn [env]
        (p/-mbind inner-monad
                     (mr env)
                     (fn [a]
                       ((f a) env)))))

    p/MonadTrans
    (-base [_]
      context)

    (-inner [_]
      inner-monad)

    (-lift [m mv]
      (fn [_]
        mv))

    MonadReader
    (-ask [_]
      (fn [env]
        (p/-mreturn inner-monad env)))

    (-local [_ f mr]
      (fn [env]
        (mr (f env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader monad functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-reader
  "Given a Reader instance, execute the
  wrapped computation and returns a value."
  [reader seed]
  (ctx/with-context context
    (reader seed)))

(def ask
  (reader
   (fn [env]
     (let [ctx (ctx/get-current context)]
       ((-ask ctx) env)))))

(def local
  (fn [f mr]
    (reader
     (fn [env]
       (let [ctx (ctx/get-current context)]
         ((-local ctx f mr) env))))))
