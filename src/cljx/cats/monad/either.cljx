(ns cats.monad.either
  "The Either (Error) Monad."
  (:require [cats.protocols :as proto]))

(declare either-monad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructor and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Either [v type]
  proto/Monadic
  (monad [_]
    either-monad)

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
      false)))

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
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def either-monad
 (reify
   proto/Functor
   (fmap [_ f s]
     (if (right? s)
       (right (f (.-v s)))
       s))

   proto/Applicative
   (pure [_ v]
     (right v))

   (fapply [m af av]
     (if (right? af)
       (proto/fmap m (.-v af) av)
       af))

   proto/Monad
   (mreturn [_ v]
     (right v))

   (mbind [_ s f]
     (if (right? s)
       (f (.-v s))
       s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad transformer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn either-trans [inner-m]
  (reify
    proto/Monad
    (mreturn [_ v]
      (proto/mreturn inner-m (right v)))

    (mbind [_ mv f]
      (proto/mbind inner-m
                   mv
                   (fn [either-v]
                     (if (left? either-v)
                       (proto/mreturn inner-m either-v)
                       (f (from-either either-v))))))

    proto/MonadTrans
    (inner [_]
      inner-m)

    (lift [m mv]
      (proto/mbind inner-m
                   mv
                   (fn [v]
                     (proto/mreturn inner-m (right v)))))))
