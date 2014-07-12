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

(ns cats.types
  "Monadic built-in types definition."
  (:require [clojure.set :as s]
            [cats.protocols :as proto]))

; TODO: set, lazy seq and list

(def vector-m
  (reify
    proto/Functor
    (fmap [_ f v]
      (vec (map f v)))

    proto/Applicative
    (pure [_ v]
      [v])

    (fapply [_ self av]
      (vec (for [f self
                 v av]
             (f v))))

    proto/Monad
    (mreturn [_ v]
      [v])

    (mbind [_ self f]
      (into []
            (apply concat (map f self))))

    proto/MonadZero
    (mzero [_]
      [])

    proto/MonadPlus
    (mplus [_ mv mv']
      (into mv mv'))))

(extend-type #+clj clojure.lang.PersistentVector
             #+cljs cljs.core.PersistentVector
  proto/Monadic
  (monad [_]
    vector-m))
