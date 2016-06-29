(ns cats.labs.promise
  "A promise monad that uses promesa library."
  (:require [cats.core :as m]
            [cats.context :as mc]
            [cats.protocols :as mp]
            [promesa.core :as p]
            [promesa.impl.proto :as pp])
  #?(:clj
     (:import java.util.concurrent.CompletableFuture)))

(declare context)

(extend-type #?(:cljs p/Promise :clj CompletableFuture)
  mp/Contextual
  (-get-context [_] context)

  mp/Extract
  (-extract [it]
    (pp/-extract it)))

(def ^:no-doc context
  (reify
    mp/Context
    mp/Functor
    (-fmap [mn f mv]
      (pp/-map mv f))

    mp/Bifunctor
    (-bimap [_ err succ mv]
      (-> mv
          (pp/-map succ)
          (pp/-catch err)))

    mp/Monad
    (-mreturn [_ v]
      (pp/-promise v))

    (-mbind [mn mv f]
      (pp/-bind mv f))

    mp/Applicative
    (-pure [_ v]
      (pp/-promise v))

    (-fapply [_ pf pv]
      (pp/-map (p/all [pf pv])
               (fn [[f v]]
                 (f v))))

    mp/Semigroup
    (-mappend [_ mv mv']
      (pp/-map (m/sequence [mv mv'])
               (fn [[mvv mvv']]
                 (let [ctx (mp/-get-context mvv)]
                   (mp/-mappend ctx mvv mvv')))))))

(def ^:deprecated promise-context
  "Deprecated alias for `context`."
  context)
