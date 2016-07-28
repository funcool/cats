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

(ns cats.context
  "A cats context management."
  (:require [cats.protocols :as p]))

(def ^:dynamic *context* nil)

(defn throw-illegal-argument
  {:no-doc true :internal true}
  [^String text]
  #?(:cljs (throw (ex-info text {}))
     :clj  (throw (IllegalArgumentException. text))))

(defn context?
  "Returnt `true` if the provided value satisfies
  the Context protocol."
  [v]
  (satisfies? p/Context v))

#?(:clj
   (defmacro with-context
     "Set current context to specific monad."
     [ctx & body]
     `(do
        (when-not (context? ~ctx)
          (throw-illegal-argument
           "The provided context does not implements Context."))
        (binding [*context* ~ctx]
          ~@body))))

#?(:clj
   (defmacro with-monad
     "Semantic alias for `with-context`."
     [ctx & body]
     `(with-context ~ctx
        ~@body)))

(defn infer
  "Given an optional value infer its context. If context is already set, it
  is returned as is without any inference operation."
  {:no-doc true}
  ([]
   (when (nil? *context*)
     (throw-illegal-argument "No context is set."))
   *context*)
  ([v]
   (cond
     (not (nil? *context*))
     *context*

     (satisfies? p/Contextual v)
     (p/-get-context v)

     :else
     (throw-illegal-argument
      (str "No context is set and it can not be automatically "
           "resolved from provided value")))))

(defn get-current
  "Deprecated alias to `infer`."
  {:deprecated true}
  [& args]
  (apply infer args))


