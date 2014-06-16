(ns cats.core
  "Category Theory abstractions for Clojure"
  (:require [cats.protocols :as p]
            [cats.types :as t])
  #+cljs
  (:require-macros [cats.core :as cm]))


(def ^{:dynamic true} *m-context*)

#+clj
(defmacro with-context
  [ctx & body]
  `(binding [*m-context* ~ctx]
     ~@body))

(defn return
  "Context dependent version of pure."
  [v]
  (p/pure *m-context* v))

(defn pure
  "Takes a context type av value and any arbitrary
  value v, and return v value wrapped in a minimal
  contex of same type of av."
  [av v]
  (p/pure av v))

(defn bind
  "Given a value inside monadic context mv and any function,
  applies a function to value of mv."
  [mv f]
  (#+clj  with-context
   #+cljs cm/with-context mv
    (p/bind mv f)))

(defn mzero
  []
  (p/mzero *m-context*))

(defn mplus
  [mv mv']
  (p/mplus mv mv'))

(defn guard
  [b]
  (if b
    (return nil)
    (mzero)))

(defn fmap
  "Apply a function f to the value inside functor's fv
  preserving the context type."
  [f fv]
  (p/fmap fv f))

(defn fapply
  "Given function inside af's conext and value inside
  av's context, applies the function to value and return
  a result wrapped in context of same type of av context."
  [af av]
  (p/fapply af av))

(defn >>=
  "Performs a Haskell-style left-associative bind."
  ([mv f]
     (bind mv f))
  ([mv f & fs]
     (reduce bind mv (cons f fs))))

(defn <$>
  "Alias of fmap."
  ([f]
     (fn [fv]
       (p/fmap fv f)))
  ([f fv]
     (p/fmap fv f)))

(defn <*>
  "Performs a Haskell-style left-associative fapply."
  ([af av]
     (p/fapply af av))
  ([af av & avs]
     (reduce p/fapply af (cons av avs))))

#+clj
(defmacro mlet
  [bindings & body]
  (when-not (and (vector? bindings) (even? (count bindings)))
    (throw (IllegalArgumentException. "bindings has to be a vector with even number of elements.")))
  (if (seq bindings)
    (let [l (get bindings 0)
          r (get bindings 1)
          next-mlet `(mlet ~(subvec bindings 2) ~@body)]
      (condp = l
        :let `(let ~r ~next-mlet)
        :when `(bind (guard ~r)
                     (fn [~(gensym)]
                       ~next-mlet))
        `(bind ~r
               (fn [~l]
                 ~next-mlet))))
    `(do ~@body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monadic functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn join
  "Remove one level of monadic structure."
  [mv]
  (bind mv identity))

(defn =<<
  "Same as the two argument version of `>>=` but with the
  arguments interchanged."
  [f mv]
  (>>= mv f))

(defn >=>
  [mf mg x]
  "Left-to-right composition of monads."
  (#+clj  mlet
   #+cljs cm/mlet [a (mf x)
                   b (mg a)]
                  (return b)))

(defn <=<
  [mg mf x]
  "Right-to-left composition of monads.

  Same as `>=>` with its first two arguments flipped."
  (#+clj  mlet
   #+cljs cm/mlet [a (mf x)
                   b (mg a)]
                  (return b)))

(defn sequence-m
  [mvs]
  {:pre [(not-empty mvs)]}
  (reduce (fn [mvs mv]
             (#+clj  mlet
              #+cljs cm/mlet [v mv
                              vs mvs]
                             (return (conj vs v))))
          (#+clj  with-context
           #+cljs cm/with-context (first mvs)
            (return []))
          mvs))

(def ^{:arglist '([mf vs])}
     map-m (comp sequence-m map))

; TODO: docstring
(defn for-m
  [vs mf]
  (map-m mf vs))

(defn lift-m
  "Lifts a function to a monadic context.

      (require '[cats.types :as t])

      (def monad+ (lift-m +))

      (monad+ (t/just 1) (t/just 2))
      ;=> <Just [3]>

      (monad+ (t/just 1) (t/nothing))
      ;=> <Nothing>
  "
  [f]
  (fn [& args]
    (#+clj  mlet
     #+cljs cm/mlet [vs (sequence-m args)]
                    (return (apply f vs)))))

(defn filter-m
  "Applies a predicate to a value in a `MonadZero` instance,
  returning the identity element when the predicate yields false.

  Otherwise, returns the instance unchanged.

      (require '[cats.types :as t])

      (filter-m (partial < 2) (t/just 3))
      ;=> <Just [3]>

      (filter-m (partial < 4) (t/just 3))
      ;=> <Nothing>
  "
  [p mv]
  (#+clj  mlet
   #+cljs cm/mlet [v mv
                  :when (p v)]
                  (return v)))

(defn when-m
  "If the expression is true, returns the monadic value.

  Otherwise, yields nil in a monadic context."
  [b mv]
  (if b
    mv
    (pure mv nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State monad functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-state
  "Return State instance with computation that returns
  the current state."
  []
  (-> (fn [s] (t/pair s s))
      (t/state-t)))

(defn put-state
  "Return State instance with computation that replaces
  the current state with specified new state."
  [newstate]
  (-> (fn [s] (t/pair s newstate))
      (t/state-t)))

(defn swap-state
  [f]
  "Return State instance with computation that applies
  specified function to state and return the old state."
  (-> (fn [s] (t/pair s (f s)))
      (t/state-t)))

(defn run-state
  "Given a State instance, execute the
  wrapped computation and return Pair
  instance with result and new state.

  (def computation (mlet [x (get-state)
                          y (put-state (inc x))]
                     (return y)))

  (def initial-state 1)
  (run-state computation initial-state)

  This should be return something to: #<Pair [1 2]>"
  [state seed]
  #+clj
  (with-context state
    (state seed))
  #+cljs
  (cm/with-context state
    (state seed)))

(defn eval-state
  "Given a State instance, execute the
  wrapped computation and return the resultant
  value, ignoring the state.
  Shortly, return the first value of pair instance
  returned by `run-state` function."
  [state seed]
  (first (run-state state seed)))

(defn exec-state
  "Given a State instance, execute the
  wrapped computation and return the resultant
  state.
  Shortly, return the second value of pair instance
  returned by `run-state` function."
  [state seed]
  (second (run-state state seed)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuation monad functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-cont
  [cont]
  "Given a Continuation instance, execute the
  wrapped computation and return its value."
  (#+clj  with-context
   #+cljs cm/with-context cont
    (cont identity)))

(defn call-cc
  [f]
  (t/->Continuation
    (fn [c]
      (let [cc (fn [a] (t/->Continuation (fn [_] (c a))))]
        ((f cc) c)))))

(defn halt-cont
  [x]
  (t/->Continuation (fn [c] x)))

(defn cont-t
  "Takes a function written in a continuation-passing-style and
  creates a Continuation out of it."
  [f]
  (t/->Continuation f))
