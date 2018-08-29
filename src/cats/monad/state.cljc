(ns cats.monad.state
  (:refer-clojure :exclude [eval get])
  (:require [cats.context :as ctx :refer [*context*]]
            [cats.core :as m]
            [cats.data :as d]
            [cats.protocols :as p]
            [cats.util :as util]))

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

(defrecord State [mfn state-context]
  p/Contextual
  (-get-context [_] state-context)

  p/Extract
  (-extract [_] mfn))

(alter-meta! #'->State assoc :private true)

(defn state
  "The State type constructor.
  The purpose of State type is wrap a simple
  function that fullfill the state signature.
  It exists just for avoid extend the clojure
  function type because is very generic type."
  ([f]
   (State. f context))
  ([f state-context]
   (State. f state-context)))

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
    p/Context

    p/Extract
    (-extract [mv] (p/-extract mv))

    p/Functor
    (-fmap [_ f fv]
      (state (fn [s]
               (let [[v ns]  ((p/-extract fv) s)]
                   (d/pair (f v) ns)))))

    p/Monad
    (-mreturn [_ v]
      (state (partial d/pair v)))

    (-mbind [_ self f]
      (state (fn [s]
               (let [p          ((p/-extract self) s)
                     value    (.-fst p)
                     newstate (.-snd p)]
                 ((p/-extract (f value)) newstate)))))

    MonadState
    (-get-state [_]
      (state #(d/pair %1 %1)))

    (-put-state [_ newstate]
      (state #(d/pair % newstate)))

    (-swap-state [_ f]
      (state #(d/pair %1 (f %1))))

    p/Printable
    (-repr [_]
      #"<State>")))

(util/make-printable (type context))

(defn ^:private get-context
  "Default to context if no context set"
  []
  (if (nil? *context*)
    context
    *context*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public Api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get
  "Return a State instance with computation that returns
  the current state."
  []
  (ctx/with-context (get-context)
    (-get-state (ctx/infer))))

(defn put
  "Return a State instance with computation that replaces
  the current state with specified new state."
  [newstate]
  (ctx/with-context (get-context)
    (-put-state (ctx/infer) newstate)))

(defn swap
  "Return a State instance with computation that applies the
  specified function to state and returns the old state."
  [f]
  (ctx/with-context (get-context)
    (-swap-state (ctx/infer) f)))

(defn run
  "Given a State instance, execute the
  wrapped computation and returns a cats.data.Pair
  instance with result and new state.
    (def computation (mlet [x (get-state)
                            y (put-state (inc x))]
                       (return y)))
    (def initial-state 1)
    (run-state computation initial-state)
  This should return something to: #<Pair [1 2]>"
  [state seed]
  ((p/-extract state) seed))

(defn eval
  "Given a State instance, execute the
  wrapped computation and return the resultant
  value, ignoring the state.
  Equivalent to taking the first value of the pair instance
  returned by `run-state` function."
  [state seed]
  (first (run state seed)))

(defn exec
  "Given a State instance, execute the
  wrapped computation and return the resultant
  state.
  Equivalent to taking the second value of the pair instance
  returned by `run-state` function."
  [state seed]
  (second (run state seed)))

(defn gets
  "State monad that returns the result of applying
  a function to a state"
  [projfn]
  (m/mlet [s (get)]
    (m/return (projfn s))))

(defn wrap-fn
  "Wraps a (possibly side-effecting) function to a state monad"
  [my-fn]
  (state (fn [s]
           (d/pair (my-fn) s))))
