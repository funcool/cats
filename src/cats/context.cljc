;; Copyright (c) 2014-2015 Andrey Antukh <niwi@niwi.nz>
;; Copyright (c) 2014-2015 Alejandro Gómez <alejandro@dialelo.com>
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

(ns cats.context
  "A context management macros."
  (:require [cats.protocols :as p]))

(def ^{:dynamic true :no-doc true}
  *context* nil)

#?(:clj
   (defmacro with-context
     "Set current context to specific monad."
     [ctx & body]
     `(cond
        (satisfies? p/MonadTrans ~ctx)
        (binding [*context* ~ctx]
          ~@body)

        (satisfies? p/MonadTrans *context*)
        (do ~@body)

        :else
        (binding [*context* ~ctx]
          ~@body))))

#?(:clj
   (defmacro with-monad
     "Semantic alias for `with-context`."
     [ctx & body]
     `(with-context ~ctx
        ~@body)))

(defn get-current
  "Get current context or obtain it from
  the provided instance."
  {:no-doc true}
  ([] (get-current nil))
  ([default]
   (cond
     (not (nil? *context*))
     *context*

     (satisfies? p/Context default)
     (p/get-context default)

     (satisfies? p/Monad default)
     default

     :else
     (throw (#?(:clj IllegalArgumentException.
                :cljs js/Error.)
             "You are using return/pure/mzero function without context.")))))
