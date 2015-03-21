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
  (:require [cats.core :refer [with-monad]])
  #+cljs
  (:require-macros [cats.core :refer (with-monad)])
  (:require [cats.protocols :as proto]
            [cats.data :as d]
            [cats.core :as m]))

(declare state-monad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructors and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype State [mfn]
  proto/Context
  (get-context [_] state-monad)
  (get-value [_] mfn)

  #+clj  clojure.lang.IFn
  #+cljs cljs.core/IFn
  (#+clj invoke #+cljs -invoke [self seed]
    (mfn seed)))

(alter-meta! #'->State assoc :private true)

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
    proto/Functor
    (fmap [_ f fv]
      (state-t (fn [s]
                 (let [[v ns]  (fv s)]
                   (d/pair (f v) ns)))))

    proto/Monad
    (mreturn [_ v]
      (state-t (fn [s]
                 (d/pair v s))))

    (mbind [_ self f]
      (-> (fn [s]
            (let [p        (self s)
                  value    (.-fst p)
                  newstate (.-snd p)]
              ((f value) newstate)))
         (state-t)))

    proto/MonadState
    (get-state [_]
      (state-t (fn [s]
                 (d/pair s s))))

    (put-state [_ newstate]
      (state-t (fn [s]
                 (d/pair s newstate))))

    (swap-state [_ f]
      (-> (fn [s]
           (d/pair s (f s)))
         (state-t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad transformer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn state-transformer [inner-monad]
  (reify
    proto/Functor
    (fmap [_ f fv]
      (state-t (fn [s]
                 (let [wr (fv s)]
                   (proto/fmap inner-monad
                               (fn [[v ns]]
                                 (d/pair (f v) ns))
                               wr)))))

    proto/Monad
    (mreturn [_ v]
      (state-t (fn [s]
                 (proto/mreturn inner-monad
                                (d/pair v s)))))

    (mbind [_ self f]
      (-> (fn [s]
            (let [mp (self s)]
              (proto/mbind inner-monad
                           mp
                           (fn [[v ns]]
                             ((f v) ns)))))
         (state-t)))

    ; FIXME: Conditionally if `inner-monad` is MonadZero
    proto/MonadZero
    (mzero [_]
      (-> (fn [s]
            (proto/mzero inner-monad))
          (state-t)))

    ; FIXME: Conditionally if `inner-monad` is MonadPlus
    proto/MonadPlus
    (mplus [_ mv mv']
      (-> (fn [s]
            (proto/mplus inner-monad (mv s) (mv' s)))
          (state-t)))

    proto/MonadState
    (get-state [_]
      (state-t (fn [s]
                 (proto/mreturn inner-monad
                                (d/pair s s)))))

    (put-state [_ newstate]
      (state-t (fn [s]
                 (proto/mreturn inner-monad
                                (d/pair s newstate)))))

    (swap-state [_ f]
      (-> (fn [s]
           (proto/mreturn inner-monad
                          (d/pair s (f s))))
          (state-t)))

    proto/MonadTrans
    (base [_]
      state-monad)

    (inner [_]
      inner-monad)

    (lift [_ mv]
      (state-t (fn [s]
                 (proto/mbind inner-monad
                              mv
                              (fn [v]
                                (proto/mreturn inner-monad
                                               (d/pair v s)))))))
))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State monad functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-state
  "Return a State instance with computation that returns
  the current state."
  []
  (proto/get-state (m/get-current-context state-monad)))

(defn put-state
  "Return a State instance with computation that replaces
  the current state with specified new state."
  [newstate]
  (proto/put-state (m/get-current-context state-monad) newstate))

(defn swap-state
  "Return a State instance with computation that applies the
  specified function to state and returns the old state."
  [f]
  (proto/swap-state (m/get-current-context state-monad) f))

(defn run-state
  "Given a State instance, execute the
  wrapped computation and returns a cats.data.Pair
  instance with result and new state.

    (def computation (mlet [x (get-state)
                            y (put-state (inc x))]
                       (return y)))

    (def initial-state 1)
    (run-state computation initial-state)

  This should be return something to: #<Pair [1 2]>"
  [state seed]
  (with-monad (m/get-current-context state-monad)
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
