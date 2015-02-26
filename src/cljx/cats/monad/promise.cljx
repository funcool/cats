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
  (:require-macros [cljs.core.async.macros :refer [go]])
  #+cljs
  (:require [cljs.core.async :refer [chan put! take! <! >!] :as async]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.dispatch :as dispatch]
            [cats.protocols :as proto]
            [cats.monad.channel])
  #+clj
  (:require [clojure.core.async :refer [go chan put! take! <! >! close!] :as async]
            [clojure.core.async.impl.protocols :as impl]
            [cats.protocols :as proto]
            [cats.monad.channel])


  #+clj
  (:refer-clojure :exclude [promise when])
  #+cljs
  (:refer-clojure :exclude [when]))


(declare promise-monad)
(declare then)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn promise-buffer
  ([] (promise-buffer nil))
  ([v]
   (let [state (atom v)]
     (reify
       impl/Buffer
       (full? [_] false)
       (remove! [_] @state)
       (add!* [this item]
         (compare-and-set! state nil item)
         this)

       #+clj
       clojure.lang.Counted
       #+clj
       (count [_]
         (if (nil? @state) 0 1024))

       #+cljs
       cljs.core/ICounted
       #+cljs
       (-count [this]
         (if (nil? @state) 0 1))))))

(defn promise
  "Promise constructor."
  ([] (chan (promise-buffer)))
  ([v] (chan (promise-buffer v))))

(defn then
  "Chain promise, mainly usefull if you do not want use
  the go macro as sugar sintax."
  [p handler]
  (let [np (promise)]
    (async/take! p (fn [v]
                     (if (nil? v)
                       (async/close! np)
                       (async/put! np (handler v)))))
    np))

#+clj
(defmacro when
  [& promises]
  `(let [pr# (promise)]
     (go
       (let [rs# [~@(map (fn [p#] `(<! ~p#)) promises)]]
         (>! pr# rs#)))
     pr#))
