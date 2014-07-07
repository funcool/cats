(ns cats.types
  "Monadic types definition."
  (:require [clojure.set :as s]
            [cats.protocols :as proto]))

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

  #+clj
  (toString [self]
    (with-out-str (print [v type])))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [self other]
    (if (instance? Either other)
      (and (= v (.-v other))
           (= type (.-type other)))
      false))

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
      s))

  proto/Monad
  (bind [s f]
    (case type
      :right (f v)
      s)))

(defn left
  "Left constructor for Either type."
  ([v]
     (Either. v :left))
  ([]
     (Either. nil :left)))

(defn right
  "Right constructor for Either type."
  ([v]
     (Either. v :right))
  ([]
     (Either. nil :right)))

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

  proto/Functor
  (fmap [s f] s)

  proto/Applicative
  (pure [s v] s)
  (fapply [s av] s)

  proto/Monad
  (bind [s f] s)

  proto/MonadZero
  (mzero [_] (Nothing.))

  proto/MonadPlus
  (mplus [_ mv] mv))

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

  proto/Functor
  (fmap [s f]
    (Just. (f v)))

  proto/Applicative
  (pure [_ v]
    (Just. v))
  (fapply [_ av]
    (proto/fmap av v))

  proto/Monad
  (bind [self f]
    (f v))

  proto/MonadZero
  (mzero [_] (Nothing.))

  proto/MonadPlus
  (mplus [mv _] mv))

(defn just
  ([v]
     (Just. v))
  ([]
     (Just. nil)))

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

(defn from-maybe
  "Return inner value from maybe monad.

  Examples:
    (from-maybe (just 1))
    ;=> 1
    (from-maybe (nothing))
    ;=> nil
  "
  [mv]
  {:pre [(maybe? mv)]}
  (cond
   (just? mv) (.-v mv)
   (nothing? mv) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clojure(Script) types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: PersistenList
; TODO: document
(extend-type #+clj  clojure.lang.LazySeq
             #+cljs cljs.core.LazySeq
  proto/Functor
  (fmap [self f] (map f self))

  proto/Applicative
  (pure [_ v] (lazy-seq [v]))
  (fapply [self av]
    (for [f self
          v av]
         (f v)))

  proto/Monad
  (bind [self f]
    (apply concat (map f self))))

; TODO: test & document
(extend-type #+clj clojure.lang.PersistentVector
             #+cljs cljs.core.PersistentVector
  proto/Functor
  (fmap [self f] (vec (map f self)))

  proto/Applicative
  (pure [_ v] [v])
  (fapply [self av]
    (vec (for [f self
               v av]
           (f v))))

  proto/Monad
  (bind [self f]
    (into []
          (apply concat (map f self))))

  proto/MonadZero
  (mzero [_] [])

  proto/MonadPlus
  (mplus [mv mv'] (into mv mv')))

; TODO: test & document
(extend-type #+clj clojure.lang.PersistentHashSet
             #+cljs cljs.core.PersistentHashSet
  proto/Functor
  (fmap [self f]
    (set (map f self)))

  proto/Applicative
  (pure [_ v] #{v})

  (fapply [self av]
    (set (for [f self
               v av]
           (f v))))

  proto/Monad
  (bind [self f]
    (apply s/union (map f self)))

  proto/MonadZero
  (mzero [_] #{})

  proto/MonadPlus
  (mplus [mv mv']
    (s/union mv mv')))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pair (State monad related)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare state-t)

(deftype State [mfn]
  #+clj  clojure.lang.IFn
  #+cljs cljs.core/IFn
  (#+clj invoke #+cljs -invoke [self seed]
    (mfn seed))

  proto/Applicative
  (pure [_ v]
    (State. (fn [s] (pair v s))))
  (fapply [_ av]
    (throw #+clj (RuntimeException. "Not implemented")
           #+cljs (js/Error. "Not implemented")))

  proto/Monad
  (bind [self f]
    (-> (fn [s]
          (let [p        (mfn s)
                value    (.-fst p)
                newstate (.-snd p)]
            ((f value) newstate)))
        (state-t))))

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
;; Continuation Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Continuation [mfn]
  #+clj   clojure.lang.IFn
  #+cljs  cljs.core/IFn
  (#+clj invoke #+cljs -invoke [self f]
    (mfn f))

  proto/Applicative
  (pure [_ v]
    (Continuation. (fn [c] (c v))))
  (fapply [_ av]
    (throw (#+clj  RuntimeException.
            #+cljs js/Error.
              "Not implemented")))

  proto/Monad
  (bind [self mf]
    (Continuation. (fn [c]
                     (self (fn [v]
                             ((mf v) c)))))))

(defn continuation
  "Default constructor for continuation."
  [mfn]
  (Continuation. mfn))
