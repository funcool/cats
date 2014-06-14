(ns cats.core
  "Category Theory abstractions for Clojure"
  (:require [cats.protocols :as p])
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

#+clj
(defn bind
  "Given a value inside monadic context mv and any function,
  applies a function to value of mv."
  [mv f]
  (with-context mv
    (p/bind mv f)))

#+cljs
(defn bind
  "Given a value inside monadic context mv and any function,
  applies a function to value of mv."
  [mv f]
  (cm/with-context mv
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
  [f fv]
  (p/fmap fv f))

(defn <*>
  "Performs a Haskell-style left-associative fapply."
  ([af av]
     (p/fapply af av))
  ([af av & avs]
     (reduce p/fapply af (cons av avs))))

#+clj
(defmacro mlet
  [bindings body]
  (when-not (and (vector? bindings) (even? (count bindings)))
    (throw (IllegalArgumentException. "bindings has to be a vector with even number of elements.")))
  (if (seq bindings)
    (let [l (get bindings 0)
          r (get bindings 1)
          next-mlet `(mlet ~(subvec bindings 2) ~body)]
      (condp = l
        :let `(let ~r ~next-mlet)
        :when `(bind (guard ~r)
                     (fn [~(gensym)]
                       ~next-mlet))
        `(bind ~r
               (fn [~l]
                 ~next-mlet))))
    body))

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

#+clj
(defn >=>
  [mf mg x]
  "Left-to-right composition of monads."
  (mlet [a (mf x)
         b (mg a)]
    (return b)))

#+cljs
(defn >=>
  [mf mg x]
  "Left-to-right composition of monads."
  (cm/mlet [a (mf x)
            b (mg a)]
    (return b)))

#+clj
(defn <=<
  [mg mf x]
  "Right-to-left composition of monads.

  Same as `>=>` with its first two arguments flipped."
  (mlet [a (mf x)
         b (mg a)]
    (return b)))

#+cljs
(defn <=<
  [mg mf x]
  "Right-to-left composition of monads.

  Same as `>=>` with its first two arguments flipped."
  (cm/mlet [a (mf x)
            b (mg a)]
    (return b)))

#+clj
(defn sequence-m
  [mvs]
  {:pre [(not-empty mvs)]}
  (reduce (fn [mvs mv]
             (mlet [v mv
                    vs mvs]
               (return (conj vs v))))
          (with-context (first mvs)
            (return []))
          mvs))

#+cljs
(defn sequence-m
  [mvs]
  {:pre [(not-empty mvs)]}
  (reduce (fn [mvs mv]
             (cm/mlet [v mv
                       vs mvs]
               (return (conj vs v))))
          (cm/with-context (first mvs)
            (return []))
          mvs))

(def ^{:arglist '([mf vs])}
     map-m (comp sequence-m map))

(defn for-m
  [vs mf]
  (map-m mf vs))
