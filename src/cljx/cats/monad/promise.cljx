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
  (:require [clojure.core.async :refer [go chan put! take! <! >! close!]]
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

#+clj
(defn promise-buffer
  ([] (promise-buffer (atom nil)))
  ([state]
   (reify
     impl/Buffer
     (full? [_] false)
     (remove! [_] @state)
     (add!* [this item]
       (swap! state (fn [inner]
                       (if (nil? inner)
                         item
                         inner)))
        this)

     clojure.lang.Counted
     (count [_]
       (if (nil? @state) 0 1)))))

#+cljs
(defn promise-buffer
  ([] (promise-buffer (atom nil)))
  ([state]
   (reify
     impl/Buffer
     (full? [_] false)
     (remove! [_] @state)
     (add!* [this item]
       (swap! state (fn [inner]
                      (if (nil? inner)
                        item
                        inner)))
        this)

     cljs.core/ICounted
     (-count [this]
       (if (nil? @state) 0 1)))))

(defn promise
  "Promise constructor."
  ([] (chan (promise-buffer)))
  ([v] (chan (promise-buffer (atom v)))))

(defn then
  "Chain promise."
  [p handler]
  (let [np (promise)]
    (take! p (fn [v]
               (if (nil? v)
                 (close! np)
                 (put! np (handler v))))
           true)
    np))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; #+clj
;; (def promise-monad
;;   (reify
;;     proto/Functor
;;     (fmap [_ f mv]
;;       (then mv f))

;;     proto/Applicative
;;     (pure [_ v]
;;       (promise v))

;;     (fapply [m af av]
;;       (then af (fn [f] (then av f))))

;;     proto/Monad
;;     (mreturn [_ v]
;;       (promise v))

;;     (mbind [_ mv f]
;;       (let [np (promise)]
;;         (go
;;           (let [res1 (<! mv)
;;                 res2 (f res1)]
;;             (if (instance? Promise res2)
;;               (>! np (<! res2))
;;               (>! np res2))))
;;         np))))
