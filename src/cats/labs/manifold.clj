;; Copyright (c) 2015 Andrey Antukh <niwi@niwi.nz>
;; Copyright (c) 2015 Alejandro Gómez <alejandro@dialelo.com>
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

(ns cats.labs.manifold
  (:require [manifold.deferred :as d]
            [manifold.stream :as s]
            [cats.context :as ctx]
            [cats.core :as m]
            [cats.protocols :as p]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  deferred-context
  (reify
    p/ContextClass
    (-get-level [_] ctx/+level-default+)

    p/Functor
    (-fmap [_ f mv]
      (d/chain mv f))

    p/Applicative
    (-pure [_ v]
      (d/success-deferred v))

    (-fapply [mn af av]
      (d/chain (d/zip' af av)
               (fn [[afv avv]]
                 (afv avv))))

    p/Semigroup
    (-mappend [ctx mv mv']
      (d/chain (d/zip' mv mv')
               (fn [[mvv mvv']]
                 (let [ctx (p/-get-context mvv)]
                   (p/-mappend ctx mvv mvv')))))

    p/Monad
    (-mreturn [_ v]
      (d/success-deferred v))

    (-mbind [it mv f]
      (d/chain mv (fn [v]
                    (ctx/with-context it
                      (f v)))))))

(extend-type manifold.deferred.Deferred
  p/Context
  (-get-context [_] deferred-context))

(extend-type manifold.deferred.SuccessDeferred
  p/Context
  (-get-context [_] deferred-context))

(extend-type manifold.deferred.ErrorDeferred
  p/Context
  (-get-context [_] deferred-context))
