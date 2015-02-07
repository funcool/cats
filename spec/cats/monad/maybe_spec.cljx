(ns cats.monad.maybe-spec
  #+cljs
  (:require [speclj.core :as s :include-macros true]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [speclj.core :as s]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.core :as m]))

(s/describe "maybe-monad"
  (s/it "Basic maybe operations."
    (s/should= 1 (maybe/from-maybe (maybe/just 1)))
    (s/should= 1 (maybe/from-maybe (maybe/just 1) 42))
    (s/should= nil (maybe/from-maybe (maybe/nothing)))
    (s/should= 42 (maybe/from-maybe (maybe/nothing) 42)))

  (s/it "get-value function"
    (s/should= (p/get-value (maybe/just 1)) 1)
    (s/should= (p/get-value (maybe/nothing)) nil))

  (s/it "Test IDeref"
    (s/should= nil @(maybe/nothing))
    (s/should= 1 @(maybe/just 1)))

  (s/it "Test predicates"
    (let [m1 (maybe/just 1)]
      (s/should (maybe/maybe? m1))
      (s/should (maybe/just? m1))))

  (s/it "Test fmap"
    (let [m1 (maybe/just 1)
          m2 (maybe/nothing)]
      (s/should= (m/fmap inc m1) (maybe/just 2))
      (s/should= (m/fmap inc m2) (maybe/nothing))))

  (s/it "The first monad law: left identity"
    (s/should= (maybe/just 2)
               (m/>>= (p/mreturn maybe/maybe-monad 2) maybe/just)))

  (s/it "The second monad law: right identity"
    (s/should= (maybe/just 2)
               (m/>>= (maybe/just 2) m/return)))

  (s/it "The third monad law: associativity"
    (s/should= (m/>>= (m/mlet [x  (maybe/just 2)
                               y  (maybe/just (inc x))]
                        (m/return y))
                      (fn [y] (maybe/just (inc y))))
               (m/>>= (maybe/just 2)
                      (fn [x] (m/>>= (maybe/just (inc x))
                                     (fn [y] (maybe/just (inc y)))))))))

(def maybe-vector-transformer (maybe/maybe-transformer b/vector-monad))

(s/describe "maybe-transformer"
  (s/it "It can be combined with the effects of other monads"
    (s/should= [(maybe/just 2)]
               (m/with-monad maybe-vector-transformer
                 (m/return 2)))

    (s/should= [(maybe/just 1)
                (maybe/just 2)
                (maybe/just 2)
                (maybe/just 3)]
               (m/with-monad maybe-vector-transformer
                 (m/mlet [x [(maybe/just 0) (maybe/just 1)]
                          y [(maybe/just 1) (maybe/just 2)]]
                   (m/return (+ x y)))))

    (s/should= [(maybe/just 1)
                (maybe/just 2)
                (maybe/just 2)
                (maybe/just 3)]
               (m/with-monad maybe-vector-transformer
                 (m/mlet [x (m/lift [0 1])
                          y (m/lift [1 2])]
                   (m/return (+ x y)))))

    (s/should= [(maybe/just 1)
                (maybe/nothing)
                (maybe/just 2)
                (maybe/nothing)]
               (m/with-monad maybe-vector-transformer
                 (m/mlet [x [(maybe/just 0) (maybe/just 1)]
                          y [(maybe/just 1) (maybe/nothing)]]
                   (m/return (+ x y)))))))
