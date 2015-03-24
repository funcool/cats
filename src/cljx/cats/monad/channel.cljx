;; Copyright (c) 2014-2015, Andrey Antukh
;; Copyright (c) 2014-2015, Alejandro Gómez
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

(ns cats.monad.channel
  #+cljs
  (:require-macros [cljs.core.async.macros :refer [go]])

  #+cljs
  (:require [cljs.core.async :refer [chan put! take! <! pipe]]
            [cljs.core.async.impl.channels :as implch]
            [cljs.core.async.impl.protocols :as impl]
            [cljs.core.async.impl.dispatch :as dispatch]
            [cats.core :as m :include-macros true]
            [cats.protocols :as proto])

  #+clj
  (:require [clojure.core.async :refer [go chan put! take! <! pipe]]
            [clojure.core.async.impl.protocols :as impl]
            [clojure.core.async.impl.dispatch :as dispatch]
            [cats.core :as m]
            [cats.protocols :as proto]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  channel-monad
  (reify
    proto/Functor
    (fmap [mn f mv]
      (let [ctx m/*context*
            channel (chan)]
        (take! mv (fn [v]
                    (put! channel
                          ;; Set double monad for handle properly
                          ;; monad transformers
                          (m/with-monad ctx
                            (f v)))))
        channel))

    proto/Applicative
    (pure [_ v]
      (let [channel (chan)]
        (put! channel v)
        channel))

    (fapply [mn af av]
      (go
        (let [afv (<! af)]
          (<! (proto/fmap mn afv av)))))

    proto/Monad
    (mreturn [_ v]
      (let [channel (chan)]
        (put! channel v)
        channel))

    (mbind [mn mv f]
      (let [ctx m/*context*
            ch (chan)]
        (take! mv (fn [v]
                    (m/with-monad ctx
                      (pipe (f v) ch))))
        ch))))

(extend-type #+clj clojure.core.async.impl.channels.ManyToManyChannel
             #+cljs cljs.core.async.impl.channels.ManyToManyChannel
  proto/Context
  (get-context [_] channel-monad))

(defn with-value
  "Simple helper that creates a channel and attach
  an value to it."
  ([value] (with-value value (chan)))
  ([value ch]
   (put! ch value)
   ch))
