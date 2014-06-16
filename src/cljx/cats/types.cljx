(ns cats.types
  "Monadic types definition."
  (:require [cats.protocols :as proto]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Either
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Either [v type]
  #+clj
  Object
  #+clj
  (equals [self other]
    (if (instance? Either other)
      (and (= v (.-v other))
           (= type (.-type other)))
      false))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [self other]
    (if (instance? Either other)
      (and (= v (.-v other))
           (= type (.-type other)))
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
  (= (.-type mv) :left))

(defn right?
  [mv]
  (= (.-type mv) :right))

(defn from-either
  "Return inner value of either monad."
  [mv]
  (.-v mv))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maybe
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Nothing []
  #+clj
  Object
  #+clj
  (equals [_ other]
    (instance? Nothing other))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [_ other]
    (instance? Nothing other))

  (toString [_]
    (with-out-str (print "")))

  proto/Monad
  (bind [s f] s)

  proto/MonadZero
  (mzero [_] (Nothing.))

  proto/MonadPlus
  (mplus [_ mv] mv)

  proto/Functor
  (fmap [s f] s)

  proto/Applicative
  (pure [s v] s)
  (fapply [s av] s))

(deftype Just [v]
  #+clj
  Object
  #+clj
  (equals [self other]
    (if (instance? Just other)
      (= v (.-v other))
      false))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [_ other]
    (if (instance? Just other)
      (= v (.-v other))
      false))

  (toString [self]
    (with-out-str (print [v])))

  proto/Monad
  (bind [self f]
    (f v))

  proto/MonadZero
  (mzero [_] (Nothing.))

  proto/MonadPlus
  (mplus [mv _] mv)

  proto/Functor
  (fmap [s f]
    (Just. (f v)))

  proto/Applicative
  (pure [_ v]
    (Just. v))
  (fapply [_ av]
    (proto/fmap av v)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(extend-type #+clj clojure.lang.PersistentVector
             #+cljs cljs.core.PersistentVector
  proto/Monad
  (bind [self f]
    (vec (flatten (map f self))))

  proto/MonadZero
  (mzero [_] [])

  proto/MonadPlus
  (mplus [mv mv'] (into mv mv'))

  proto/Functor
  (fmap [self f] (vec (map f self)))

  proto/Applicative
  (pure [_ v] [v])
  (fapply [self av]
    (vec (for [f self
               v av]
           (f v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pair (State monad related)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clj
(deftype Pair [fst snd]
  clojure.lang.Seqable
  (seq [_] (list fst snd))

  clojure.lang.Indexed
  (nth [_ i]
    (case i
      0 fst
      1 snd
      (throw (IndexOutOfBoundsException.))))

  (nth [_ i notfound]
    (case i
      0 fst
      1 snd
      notfound))

  clojure.lang.Counted
  (count [_] 2)

  Object
  (equals [this other]
    (if (instance? Pair other)
      (and (= (.-fst this) (.-fst other))
           (= (.-snd this) (.-snd other)))
      false))

  (toString [this]
    (with-out-str (print [fst snd]))))

#+clj
(defn pair
  [fst snd]
  (Pair. fst snd))

#+clj
(defn pair?
  [v]
  (instance? Pair v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clj
(declare state-t)

#+clj
(deftype State [mfn]
  proto/Monad
  (bind [self f]
    (-> (fn [s]
          (let [p        (mfn s)
                value    (.-fst p)
                newstate (.-snd p)]
            ((f value) newstate)))
        (state-t)))

  clojure.lang.IFn
  (invoke [self seed]
    (mfn seed))

  proto/Applicative
  (pure [_ v]
    (State. (fn [s] (pair v s))))
  (fapply [_ av]
    (throw (RuntimeException. "Not implemented"))))

#+clj
(defn state-t
  "Transform a simple state-monad function
  to State class instance.
  State class instance work as simple wrapper
  for standard clojure function, just for avoid
  extend plain function type of clojure."
  [f]
  (State. f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuation Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Continuation [mfn]
  proto/Monad
  (bind [self mf]
    (Continuation. (fn [c]
                     (self (fn [v]
                             ((mf v) c))))))

  #+clj   clojure.lang.IFn
  #+cljs  cljs.core/IFn
  (invoke [self f]
    (mfn f))

  proto/Applicative
  (pure [_ v]
    (Continuation. (fn [c] (c v))))
  (fapply [_ av]
    (throw (RuntimeException. "Not implemented"))))
