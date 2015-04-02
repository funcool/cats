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

(ns cats.monad.writer
  "The Writer Monad."
  #+clj
  (:require [cats.core :refer [with-monad]])
  #+cljs
  (:require-macros [cats.core :refer (with-monad)])
  (:require [cats.protocols :as proto]
            [cats.builtin :as b]
            [cats.core :as m]
            [cats.data :as d]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  writer-monad
  (reify
    proto/Monad
    (mreturn [_ v]
      (d/pair v (proto/mempty b/vector-monad)))

    (mbind [_ mv f]
      (let [[v log] mv
            [v' log'] (f v)]
        (d/pair v' (proto/mappend (proto/get-context log) log log'))))

    proto/MonadWriter
    (tell [_ v]
      (d/pair nil [v]))

    (listen [_ mv]
      (d/pair mv (second mv)))

    (pass [_ mv]
      (let [[v f] (first mv)]
        (d/pair v (f (second mv)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad transformer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn writer-transformer
  "The Writer transformer constructor."
  [inner-monad]
  (reify
    proto/Monad
    (mreturn [_ v]
      (proto/mreturn inner-monad
                     (d/pair v (proto/mempty b/vector-monad))))

    (mbind [_ mv f]
      (proto/mbind inner-monad
                   mv
                   (fn [[v log]]
                    (proto/mbind inner-monad
                                 (f v)
                                 (fn [[v' log']]
                                   (proto/mreturn inner-monad
                                                  (d/pair v' (proto/mappend (proto/get-context log) log log'))))))))

    proto/MonadWriter
    (tell [_ v]
      (proto/mreturn inner-monad (d/pair nil [v])))

    (listen [_ mv]
      (proto/mbind inner-monad
                   mv
                   (fn [mv]
                     (proto/mreturn inner-monad
                                    (d/pair mv (second mv))))))

    (pass [_ mv]
      (proto/mbind inner-monad
                   mv
                   (fn [w]
                     (let [[v f] (first w)
                           log   (second w)]
                       (proto/mreturn inner-monad
                                      (d/pair v (f log)))))))

    proto/MonadTrans
    (base [_]
      writer-monad)

    (inner [_]
      inner-monad)

    (lift [_ mv]
      (proto/mbind inner-monad
                   mv
                   (fn [v]
                     (proto/mreturn inner-monad
                                    (d/pair v (proto/mempty b/vector-monad))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writer monad functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tell
  "Add the value to the log."
  [v]
  (proto/tell (m/get-current-context writer-monad) v))

(defn listen
  "Get the value from the log."
  [mv]
  (proto/listen (m/get-current-context writer-monad) mv))

(defn pass
  [mv]
  "Apply a function to the log."
  (proto/pass (m/get-current-context writer-monad) mv))

(def value first)
(def log second)
