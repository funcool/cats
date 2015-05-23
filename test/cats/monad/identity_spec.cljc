(ns cats.monad.identity-spec
  #?(:cljs
     (:require [cljs.test :as t]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.monad.identity :as id]
               [cats.core :as m :include-macros true])
     :clj
     (:require [clojure.test :as t]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.monad.identity :as id]
               [cats.core :as m])))

(t/deftest basic-operations-test
  (t/is (= 1 @(id/identity 1))))

(t/deftest functor-test
  (t/testing "Test fmap"
    (t/is (= (id/identity 2)
             (m/fmap inc (id/identity 1))))))

(t/deftest first-monad-law-left-identity
  (t/is (= (id/identity 2)
           (m/>>= (p/mreturn id/identity-monad 2) id/identity))))

(t/deftest second-monad-law-right-identity
  (t/is (= (id/identity 2)
           (m/>>= (id/identity 2) m/return))))

(t/deftest third-monad-law-associativity
  (t/is (= (m/>>= (m/mlet [x (id/identity 2)
                           y (id/identity (inc x))]
                    (m/return y))
                  (fn [y] (id/identity (inc y))))
           (m/>>= (id/identity 2)
                  (fn [x] (m/>>= (id/identity (inc x))
                                 (fn [y] (id/identity (inc y)))))))))

(def identity-vecotor-t (id/identity-transformer b/vector-monad))

(t/deftest identity-transformer-tests
  (t/is (= (id/identity [2])
           (m/with-monad identity-vecotor-t
             (m/return 2)))))
