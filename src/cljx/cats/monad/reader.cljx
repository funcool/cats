;; Copyright (c) 2014, Andrey Antukh
;; Copyright (c) 2014, Alejandro Gómez
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

(ns cats.monad.reader
  "The Reader Monad."
  #+clj
  (:require [cats.core :refer [with-monad]])
  #+cljs
  (:require-macros [cats.core :refer (with-monad)])
  (:require [cats.protocols :as proto]
            [cats.core :as m]))

(declare reader-monad)

(deftype Reader [mfn]
  proto/Context
  (get-context [_] reader-monad)
  (get-value [_] mfn)

  #+clj  clojure.lang.IFn
  #+cljs cljs.core/IFn
  (#+clj invoke #+cljs -invoke [self seed]
    (mfn seed)))

(defn reader
  "Transform a simple reader-monad function
  to Reader class instance.
  Reader class instance work as simple wrapper
  for standard clojure function, just for avoid
  extend plain function type of clojure."
  [f]
  (Reader. f))

(defn reader?
  "Check if value s is instance of
  Reader type."
  [s]
  (instance? Reader s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def reader-monad
  (reify
    proto/Functor
    (fmap [_ f fv]
      (reader (fn [env]
                (f (fv env)))))

    proto/Monad
    (mreturn [_ v]
      (reader (fn [env]
                v)))

    (mbind [_ mv f]
      (reader (fn [env]
                ((f (mv env)) env))))

    proto/MonadReader
    (ask [_]
      (reader (fn [env]
                env)))

    (local [_ f mr]
      (reader (fn [env]
                (mr (f env)))))

  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad transformer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn reader-transformer [inner-monad]
  (reify
    proto/Functor
    (fmap [_ f fv]
      (reader (fn [env]
                (proto/fmap inner-monad f (fv env)))))

    proto/Monad
    (mreturn [_ v]
      (proto/mreturn reader-monad (proto/mreturn inner-monad v)))

    (mbind [_ mr f]
      (fn [env]
        (proto/mbind inner-monad
                     (mr env)
                     (fn [a]
                       ((f a) env)))))

    proto/MonadTrans
    (base [_]
      reader-monad)

    (inner [_]
      inner-monad)

    (lift [m mv]
      (fn [_]
        mv))

    proto/MonadReader
    (ask [_]
      (fn [env]
        (proto/mreturn inner-monad env)))

    (local [_ f mr]
      (fn [env]
        (mr (f env))))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reader monad functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-reader
  "Given a Reader instance, execute the
  wrapped computation and returns a value."
  [reader seed]
  (with-monad (m/get-current-context reader-monad)
    (reader seed)))

(def ask
  (reader
   (fn [env]
     (let [ctx (m/get-current-context reader-monad)]
       ((proto/ask  ctx) env)))))

(def local
  (fn [f mr]
    (reader
     (fn [env]
       (let [ctx (m/get-current-context reader-monad)]
         ((proto/local ctx f mr) env))))))
