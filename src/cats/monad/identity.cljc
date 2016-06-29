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

(ns cats.monad.identity
  "The Identity Monad."
  (:refer-clojure :exclude [identity])
  (:require [cats.protocols :as p]
            [cats.context :as ctx]
            [cats.util :as util]))

(declare context)

;; --- Type constructors

(deftype Identity [v]
  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] v)

  p/Printable
  (-repr [_]
    (str "#<Identity " (pr-str v) ">"))

  #?@(:cljs [cljs.core/IDeref
             (-deref [_] v)]
      :clj  [clojure.lang.IDeref
             (deref [_] v)])

  #?@(:clj
      [Object
       (equals [self other]
         (if (instance? Identity other)
           (= v (.-v ^Identity other))
           false))]

      :cljs
      [cljs.core/IEquiv
       (-equiv [_ other]
         (if (instance? Identity other)
           (= v (.-v ^Identity other))
           false))]))

(alter-meta! #'->Identity assoc :private true)

(util/make-printable Identity)

(defn identity
  "The Identity type constructor."
  [v]
  (Identity. v))

;; --- Monad definition

(def ^{:no-doc true}
  context
  (reify
    p/Context
    p/Functor
    (-fmap [_ f iv]
      (Identity. (f (.-v ^Identity iv))))

    p/Applicative
    (-pure [_ v]
      (Identity. v))

    (-fapply [_ af av]
      (Identity. ((.-v ^Identity af) (.-v ^Identity av))))

    p/Monad
    (-mreturn [_ v]
      (Identity. v))

    (-mbind [_ mv f]
      (f (.-v ^Identity mv)))

    p/Printable
    (-repr [_]
      "#<Identity>")))

(util/make-printable (type context))
