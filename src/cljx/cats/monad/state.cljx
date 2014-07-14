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

(ns cats.monad.state
  "The State Monad."
  #+clj
  (:require [cats.core :refer [with-context]])
  #+cljs
  (:require-macros [cats.core :refer (with-context)])
  (:require [cats.protocols :as proto]))

(declare state-monad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructors and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Pair [fst snd]
  #+clj  clojure.lang.Seqable
  #+cljs cljs.core/ISeqable
  (#+cljs -seq #+clj seq [_]
    (list fst snd))

  #+clj  clojure.lang.Indexed
  #+cljs cljs.core/IIndexed
  (#+clj nth #+cljs -nth [_ i]
    (case i
      0 fst
      1 snd
      (throw #+clj (IndexOutOfBoundsException.)
             #+cljs (js/Error. "Out of index"))))

  (#+clj nth #+cljs -nth [_ i notfound]
    (case i
      0 fst
      1 snd
      notfound))

  #+clj  clojure.lang.Counted
  #+cljs cljs.core/ICounted
  (#+clj count #+cljs -count [_] 2)

  #+clj  java.lang.Object
  #+cljs cljs.core/IEquiv
  (#+clj equals #+cljs -equiv [this other]
    (if (instance? Pair other)
      (and (= (.-fst this) (.-fst other))
           (= (.-snd this) (.-snd other)))
      false))

  #+clj
  (toString [this]
    (with-out-str (print [fst snd]))))

(defn pair
  [fst snd]
  (Pair. fst snd))

(defn pair?
  [v]
  (instance? Pair v))

(deftype State [mfn]
  proto/Context
  (get-context [_] state-monad)
  (get-value [_] mfn)

  #+clj  clojure.lang.IFn
  #+cljs cljs.core/IFn
  (#+clj invoke #+cljs -invoke [self seed]
    (mfn seed)))

(defn state-t
  "Transform a simple state-monad function
  to State class instance.
  State class instance work as simple wrapper
  for standard clojure function, just for avoid
  extend plain function type of clojure."
  [f]
  (State. f))

(defn state?
  "Check if value s is instance of
  State type."
  [s]
  (instance? State s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def state-monad
  (reify
    proto/Functor ; TODO: test functor laws
    (fmap [_ f fv]
      (state-t (fn [s]
                 (f (fv s)))))

    proto/Monad
    (mreturn [_ v]
      (state-t (fn [s]
                 (pair v s))))

    (mbind [_ self f]
      (-> (fn [s]
            (let [p        (self s)
                  value    (.-fst p)
                  newstate (.-snd p)]
              ((f value) newstate)))
          (state-t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State monad functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-state
  "Return a State instance with computation that returns
  the current state."
  []
  (-> (fn [s] (pair s s))
     (state-t)))

(defn put-state
  "Return a State instance with computation that replaces
  the current state with specified new state."
  [newstate]
  (-> (fn [s] (pair s newstate))
     (state-t)))

(defn swap-state
  "Return a State instance with computation that applies the
  specified function to state and returns the old state."
  [f]
  (-> (fn [s] (pair s (f s)))
     (state-t)))

(defn run-state
  "Given a State instance, execute the
  wrapped computation and returns a Pair
  instance with result and new state.

    (def computation (mlet [x (get-state)
                            y (put-state (inc x))]
                       (return y)))

    (def initial-state 1)
    (run-state computation initial-state)

  This should be return something to: #<Pair [1 2]>"
  [state seed]
  (with-context state-monad
    (state seed)))

(defn eval-state
  "Given a State instance, execute the
  wrapped computation and return the resultant
  value, ignoring the state.
  Equivalent to taking the first value of the pair instance
  returned by `run-state` function."
  [state seed]
  (first (run-state state seed)))

(defn exec-state
  "Given a State instance, execute the
  wrapped computation and return the resultant
  state.
  Equivalent to taking the second value of the pair instance
  returned by `run-state` function."
  [state seed]
  (second (run-state state seed)))

; TODO: State transformer
