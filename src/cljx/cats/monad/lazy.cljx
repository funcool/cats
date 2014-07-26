(ns cats.monad.lazy
  "The Lazy Monad."
  #+clj
  (:require [cats.protocols :as proto]
            [cats.core :as m :refer (with-context)])
  #+cljs
  (:require-macros [cats.core :refer (with-context)])
  #+cljs
  (:require [cats.protocols :as proto]
            [cats.core :as m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delay/Lazy Monad
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def lazy-monad
  (reify
    proto/Functor
    (fmap [_ f fv]
      (delay (f (proto/get-value fv))))

    proto/Applicative
    (pure [_ v]
      (delay v))

    (fapply [m af av]
      (proto/fmap m (deref af) av))

    proto/Monad
    (mbind [m self f]
      (delay
       (with-context m
         (proto/get-value (f @self)))))

    (mreturn [_ v]
      (delay v))))

;; TODO: lazy monad transformer

(extend-type #+clj clojure.lang.Delay
             #+cljs cljs.core.Delay
  proto/Context
  (get-context [_] lazy-monad)
  (get-value [self] @self))
