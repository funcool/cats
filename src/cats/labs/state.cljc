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

(ns cats.labs.state
  "The State Monad."
  #?(:cljs (:require [cats.protocols :as p]
                     [cats.context :as ctx :include-macros true]
                     [cats.data :as d])
     :clj  (:require [cats.protocols :as p]
                     [cats.context :as ctx]
                     [cats.data :as d])))

(declare context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol declaration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol MonadState
  "A specific case of Monad abstraction for
  work with state in pure functional way."
  (-get-state [m] "Return the current state.")
  (-put-state [m newstate] "Update the state.")
  (-swap-state [m f] "Apply a function to the current state and update it."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructors and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype State [mfn]
  p/Context
  (-get-context [_] context)

  #?@(:cljs [cljs.core/IFn
             (-invoke [self seed]
               (mfn seed))]
      :clj  [clojure.lang.IFn
             (invoke [self seed]
               (mfn seed))]))

(alter-meta! #'->State assoc :private true)

(defn state-t
  "The State type constructor.

  The purpose of State type is wrap a simple
  function that fullfill the state signature.

  It exists just for avoid extend the clojure
  function type because is very generic type."
  [f]
  (State. f))

(defn state?
  "Return true if `s` is instance of
  the State type."
  [s]
  (instance? State s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^{:no-doc true}
  context
  (reify
    p/ContextClass
    (-get-level [_] ctx/+level-default+)

    p/Functor
    (-fmap [_ f fv]
      (state-t (fn [s]
                 (let [[v ns]  (fv s)]
                   (d/pair (f v) ns)))))

    p/Monad
    (-mreturn [_ v]
      (state-t (partial d/pair v)))

    (-mbind [_ self f]
      (state-t (fn [s]
                 (let [p        (self s)
                       value    (.-fst p)
                       newstate (.-snd p)]
                   ((f value) newstate)))))

    MonadState
    (-get-state [_]
      (state-t #(d/pair %1 %1)))

    (-put-state [_ newstate]
      (state-t #(d/pair % newstate)))

    (-swap-state [_ f]
      (state-t #(d/pair %1 (f %1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad transformer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn state-transformer
  "The State transformer constructor."
  [inner-monad]
  (reify
    p/ContextClass
    (-get-level [_] ctx/+level-transformer+)

    p/Functor
    (-fmap [_ f fv]
      (state-t (fn [s]
                 (let [wr (fv s)]
                   (p/-fmap inner-monad
                            (fn [[v ns]]
                              (d/pair (f v) ns))
                            wr)))))

    p/Monad
    (-mreturn [_ v]
      (state-t (fn [s]
                 (p/-mreturn inner-monad
                             (d/pair v s)))))

    (-mbind [_ self f]
      (state-t (fn [s]
                 (let [mp (self s)]
                   (p/-mbind inner-monad
                             mp
                             (fn [[v ns]]
                               ((f v) ns)))))))

                                        ; FIXME: Conditionally if `inner-monad` is MonadZero
    p/MonadZero
    (-mzero [_]
      (state-t (fn [s]
                 (p/-mzero inner-monad))))

                                        ; FIXME: Conditionally if `inner-monad` is MonadPlus
    p/MonadPlus
    (-mplus [_ mv mv']
      (state-t (fn [s]
                 (p/-mplus inner-monad (mv s) (mv' s)))))

    MonadState
    (-get-state [_]
      (state-t (fn [s]
                 (p/-mreturn inner-monad
                             (d/pair s s)))))

    (-put-state [_ newstate]
      (state-t (fn [s]
                 (p/-mreturn inner-monad
                             (d/pair s newstate)))))

    (-swap-state [_ f]
      (state-t (fn [s]
                 (p/-mreturn inner-monad
                             (d/pair s (f s))))))

    p/MonadTrans
    (-base [_]
      context)

    (-inner [_]
      inner-monad)

    (-lift [_ mv]
      (state-t (fn [s]
                 (p/-mbind inner-monad
                           mv
                           (fn [v]
                             (p/-mreturn inner-monad
                                         (d/pair v s)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-state
  "Return a State instance with computation that returns
  the current state."
  []
  (-get-state (ctx/get-current context)))

(defn put-state
  "Return a State instance with computation that replaces
  the current state with specified new state."
  [newstate]
  (-put-state (ctx/get-current context) newstate))

(defn swap-state
  "Return a State instance with computation that applies the
  specified function to state and returns the old state."
  [f]
  (-swap-state (ctx/get-current context) f))

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
  (ctx/with-context context
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
