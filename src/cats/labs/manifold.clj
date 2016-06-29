;; Copyright (c) 2015-2016 Andrey Antukh <niwi@niwi.nz>
;; Copyright (c) 2015-2016 Alejandro Gómez <alejandro@dialelo.com>
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
            [cats.protocols :as p]
            [cats.monad.either :as either]
            [cats.util :as util]))

(defn- with-timeout
  [timeout timeout-value d]
  (when timeout
    (if timeout-value
      (d/timeout! d timeout timeout-value)
      (d/timeout! d timeout)))
  d)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype DeferredContext [timeout timeout-value]
  Object
  (equals [self other]
    ;; Two context with different values are considered
    ;; equals because as context they are represent
    ;; the same context.
    ;; FIXME: abstract this using own abstraction
    ;; without overriding the default equality checks.
    (instance? DeferredContext other))

  p/Context
  p/Functor
  (-fmap [_ f mv]
    (with-timeout timeout timeout-value
      (d/chain mv f)))

  p/Applicative
  (-pure [_ v]
    (d/success-deferred v))

  (-fapply [mn af av]
    (with-timeout timeout timeout-value
      (d/chain (d/zip' af av)
               (fn [[afv avv]]
                 (afv avv)))))

  p/Semigroup
  (-mappend [ctx mv mv']
    (with-timeout timeout timeout-value
      (d/chain (d/zip' mv mv')
               (fn [[mvv mvv']]
                 (let [ctx (p/-get-context mvv)]
                   (p/-mappend ctx mvv mvv'))))))

  p/Monad
  (-mreturn [_ v]
    (d/success-deferred v))

  (-mbind [it mv f]
    (with-timeout timeout timeout-value
      (d/chain mv (fn [v]
                    (ctx/with-context it
                      (f v))))))

  p/Printable
  (-repr [_]
    "#<Deferred>"))

(defn deferred-context*
  ([] (deferred-context* nil nil))
  ([timeout] (deferred-context* timeout nil))
  ([timeout timeout-value]
   (DeferredContext. timeout timeout-value)))

(def ^{:doc "A default context for manifold deferred."}
  deferred-context (deferred-context*))

(util/make-printable (type deferred-context))

(extend-type manifold.deferred.Deferred
  p/Contextual
  (-get-context [_] deferred-context))

(extend-type manifold.deferred.SuccessDeferred
  p/Contextual
  (-get-context [_] deferred-context))

(extend-type manifold.deferred.ErrorDeferred
  p/Contextual
  (-get-context [_] deferred-context))
