(ns cats.monad.lazy
  "The Lazy Monad."
  #+clj
  (:require [cats.protocols :as proto]
            [cats.core :as m :refer (with-monad)])
  #+cljs
  (:require-macros [cats.core :refer (with-monad)])
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
       (with-monad m
         (proto/get-value (f @self)))))

    (mreturn [_ v]
      (delay v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad transformer definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn lazy-transformer [inner-monad]
  (reify
    proto/Functor
    (fmap [_ f fv]
      (proto/fmap lazy-monad
                  (partial proto/fmap inner-monad f)
                  fv))
    proto/Monad
    (mreturn [_ v]
      (->> (proto/mreturn inner-monad v)
           (proto/mreturn lazy-monad)))

    (mbind [_ mv f]
      (proto/mbind lazy-monad
                   mv
                   (fn [mv']
                     (proto/mbind inner-monad mv' f))))

    proto/MonadTrans
    (base [_]
      lazy-monad)

    (inner [_]
      inner-monad)

    (lift [_ mv]
      (proto/mbind inner-monad
                   mv
                   (fn [v]
                     (->> (proto/mreturn inner-monad v)
                          (proto/mreturn lazy-monad)))))))

(extend-type #+clj clojure.lang.Delay
             #+cljs cljs.core.Delay
  proto/Context
  (get-context [_] lazy-monad)
  (get-value [self] @self))
