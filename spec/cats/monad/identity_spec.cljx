(ns cats.monad.identity-spec
  #+cljs
  (:require [speclj.core :as s :include-macros true]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.identity :as id]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [speclj.core :as s]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.identity :as id]
            [cats.core :as m]))

(s/describe "identity-monad"
  (s/it "Test IDeref"
    (s/should= 1 @(id/identity 1)))

  (s/it "Test fmap"
    (s/should= (id/identity 2)
               (m/fmap inc (id/identity 1))))

  (s/it "The first monad law: left identity"
    (s/should= (id/identity 2)
               (m/>>= (p/mreturn id/identity-monad 2) id/identity)))

  (s/it "The second monad law: right identity"
    (s/should= (id/identity 2)
               (m/>>= (id/identity 2) m/return)))

  (s/it "The third monad law: associativity"
    (s/should= (m/>>= (m/mlet [x (id/identity 2)
                               y (id/identity (inc x))]
                        (m/return y))
                      (fn [y] (id/identity (inc y))))
               (m/>>= (id/identity 2)
                      (fn [x] (m/>>= (id/identity (inc x))
                                     (fn [y] (id/identity (inc y)))))))))
(def identity-vector-transformer (id/identity-transformer b/vector-monad))

(s/describe "identity-transformer"
  (s/it "It is a trivial transformer which yields the inner monad"
    (s/should= (id/identity [2])
               (m/with-monad identity-vector-transformer
                 (m/return 2)))))
