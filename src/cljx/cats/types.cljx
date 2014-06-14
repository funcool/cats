(ns cats.types
  "Monadic types definition."
  (:require [cats.protocols :as proto]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Either
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Either [v type]
  Object
  (equals [self other]
    (if (instance? Either other)
      (and (= v (.v other))
           (= type (.type other)))
      false))

  (toString [self]
    (with-out-str (print [v type])))

  proto/Monad
  (bind [s f]
    (case type
      :right (f v)
      s))

  proto/Functor
  (fmap [s f]
    (case type
      :right (Either. (f v) :right)
      s))

  proto/Applicative
  (pure [_ v]
    (Either. v type))
  (fapply [s av]
    (case type
      :right (proto/fmap av v)
      s)))

(defn left
  "Left constructor for Either type."
  [^Object v]
  (Either. v :left))

(defn right
  "Right constructor for Either type."
  [^Object v]
  (Either. v :right))

(defn left?
  [mv]
  (= (.type mv) :left))

(defn right?
  [mv]
  (= (.type mv) :right))

(defn from-either
  "Return inner value of either monad."
  [mv]
  (.v mv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maybe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Just [v]
  Object
  (equals [self other]
    (if (instance? Just other)
      (= v (.v other))
      false))

  (toString [self]
    (with-out-str (print [v])))

  proto/Monad
  (bind [self f]
    (f v))

  proto/Functor
  (fmap [s f]
    (Just. (f v)))

  proto/Applicative
  (pure [_ v]
    (Just. v))
  (fapply [_ av]
    (proto/fmap av v)))

(deftype Nothing []
  Object
  (equals [_ other]
    (instance? Nothing other))

  (toString [_]
    (with-out-str (print "")))

  proto/Monad
  (bind [s f] s)

  proto/Functor
  (fmap [s f] s)

  proto/Applicative
  (pure [s v] s)
  (fapply [s av] s))

(defn just
  [v]
  (Just. v))

(defn nothing
  []
  (Nothing.))

(defn maybe?
  [v]
  (or (instance? Just v)
      (instance? Nothing v)))

(defn just?
  [v]
  (instance? Just v))

(defn nothing?
  [v]
  (instance? Nothing v))
