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

(ns cats.monad.promise
  #+cljs
  (:require-macros [cljs.core.async.macros :refer [go]]
                   [cats.core :refer [with-monad]])
  #+cljs
  (:require [cljs.core.async :refer [chan put! take! <! >!]]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.dispatch :as dispatch]
            [cats.protocols :as proto])
  #+clj
  (:require [clojure.core.async :refer [go chan put! take! <! >!]]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.core.async.impl.channels :refer [box]]
            [clojure.core.async.impl.dispatch :as dispatch]
            [clojure.core.async.impl.mutex :as mutex]
            [cats.core :refer [with-monad]]
            [cats.protocols :as proto])

  #+clj
  (:refer-clojure :exclude [promise])

  #+clj
  (:import java.util.LinkedList
           java.util.concurrent.locks.Lock))

(declare promise-monad)
(declare then)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Promise [^LinkedList takes ^Lock mutex value state]
  proto/Context
  (get-context [_] promise-monad)
  (get-value [_] @value)

  Object
  (toString [_]
    (with-out-str
      (print @state)))

  impl/ReadPort
  (take! [_ handler]
    (.lock mutex)
    (let [s @state]
      (condp = s
        ::resolved
        (do
          (.unlock mutex)
          (dispatch/run #((impl/commit handler) @value))
          nil)

        ::unresolved
        (do
          (.add takes handler)
          (.unlock mutex)
          nil)

        (do
          (.unlock mutex)
          nil))))

  impl/WritePort
  (put! [this val handler]
    (.lock mutex)
    (let [s @state]
      (condp = s
        ::resolved
        (do
          (dispatch/run #((impl/commit handler) @value))
          (.unlock mutex)
          (box false))

        ::unresolved
        (let [takesv (into [] takes)]
          (.clear takes)
          (vreset! value val)
          (vreset! state ::resolved)

          (doseq [takefn takesv]
            (dispatch/run #((impl/commit takefn) val)))

          (dispatch/run #((impl/commit handler) val))
          (.unlock mutex)
          (box true))

        (do
          (.unlock mutex)
          (box false))))))

(defn promise
  "Promise constructor."
  ([] (promise nil ::unresolved))
  ([v] (promise v ::resolved))
  ([v t] (Promise. (LinkedList.)
                   (mutex/mutex)
                   (volatile! v)
                   (volatile! t))))

(defn resolved?
  [^Promise p]
  (= ::resolved @(.-state p)))

(defn then
  "Chain promise."
  [^Promise p handler]
  (let [np (promise)]
    (go
      (let [res1 (<! p)
            res2 (handler res1)]
        (put! np res2)))
    np))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def promise-monad
  (reify
    proto/Functor
    (fmap [_ f mv]
      (then mv f))

    proto/Applicative
    (pure [_ v]
      (promise v))

    (fapply [m af av]
      (then af (fn [f] (then av f))))

    proto/Monad
    (mreturn [_ v]
      (promise v))

    (mbind [_ mv f]
      (let [np (promise)]
        (go
          (let [res1 (<! mv)
                res2 (f res1)]
            (if (instance? Promise res2)
              (>! np (<! res2))
              (>! np res2))))
        np))))
