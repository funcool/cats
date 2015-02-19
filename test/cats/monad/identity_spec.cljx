(ns cats.monad.identity-spec
  #+cljs
  (:require [cljs.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.identity :as id]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [clojure.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.identity :as id]
            [cats.core :as m]))

(t/deftest identity-monad-tests
  (t/testing "Test IDeref"
    (t/is (= 1 @(id/identity 1))))

  (t/testing "Test fmap"
    (t/is (= (id/identity 2)
             (m/fmap inc (id/identity 1)))))

  (t/testing "The first monad law: left identity"
    (t/is (= (id/identity 2)
             (m/>>= (p/mreturn id/identity-monad 2) id/identity))))

  (t/testing "The second monad law: right identity"
    (t/is (= (id/identity 2)
             (m/>>= (id/identity 2) m/return))))

  (t/testing "The third monad law: associativity"
    (t/is (= (m/>>= (m/mlet [x (id/identity 2)
                             y (id/identity (inc x))]
                      (m/return y))
                    (fn [y] (id/identity (inc y))))
             (m/>>= (id/identity 2)
                    (fn [x] (m/>>= (id/identity (inc x))
                                   (fn [y] (id/identity (inc y))))))))))

(def identity-vector-transformer (id/identity-transformer b/vector-monad))

(t/deftest identity-transformer-tests
  (t/testing "It is a trivial transformer which yields the inner monad"
    (t/is (= (id/identity [2])
             (m/with-monad identity-vector-transformer
               (m/return 2))))))
