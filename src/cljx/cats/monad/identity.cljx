(ns cats.monad.identity
  "The Identity Monad."
  (:require [cats.protocols :as proto]))

(declare identity-monad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Identity [v]
  proto/Monadic
  (monad [_]
    identity-monad)

  #+clj
  Object
  #+clj
  (equals [_ other]
    (= v (.-v other)))

  #+cljs
  cljs.core/IEquiv
  #+cljs
  (-equiv [_ other]
    (= v (.-v other)))

  #+clj
  (toString [_]
    (str v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def identity-monad
  (reify
    proto/Functor
    (fmap [_ f iv]
      (Identity. (f (.-v iv))))

    proto/Applicative
    (pure [_ v]
      (Identity. v))

    (fapply [_ af av]
      (Identity. ((.-v af) (.-v av))))

    proto/Monad
    (mreturn [_ v]
      (Identity. v))

    (mbind [_ mv f]
      (f (.-v mv)))))
