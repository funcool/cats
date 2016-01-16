(ns cats.monad-spec
  #?(:cljs
     (:require [cljs.test :as t]
               [cats.builtin :as b]
               [cats.monad.maybe :as maybe]
               [cats.monad :as m :include-macros true] )
     :clj
     (:require [clojure.test :as t]
               [cats.builtin :as b]
               [cats.monad.maybe :as maybe]
               [cats.monad :as m]
               [cats.context :as ctx])))

(t/deftest simple-monadic-computations
  (let [program (m/mdo [x (m/return 2)
                        y (m/return 21)]
                  (m/return (* x y)))]
    (t/testing "Can be used with a sequence monad"
      (t/is (= [42]
               (m/mrun program b/sequence-context))))
    (t/testing "Can be used with a maybe monad"
      (t/is (= (maybe/just 42)
               (m/mrun program maybe/context))))))

(t/deftest nested-monadic-computations
  (let [program (m/mdo [x (m/return 1)
                        y (m/return 2)
                        z (m/mdo [a (m/return 1)
                                  b (m/return 2)
                                  c (m/mdo [p (m/return 2)
                                            q (m/return 2)]
                                           (m/return (* p q)))]
                             (m/return (+ a b c)))]
                  (m/return (+ x y z)))]
    (t/testing "Can be used with a sequence monad"
      (t/is (= [10]
               (m/mrun program b/sequence-context))))
    (t/testing "Can be used with a maybe monad"
      (t/is (= (maybe/just 10)
               (m/mrun program maybe/context))))))

(t/deftest inline-lets
  (let [program (m/mdo [x (m/return 1)
                        y (m/return 2)
                        :let [z (+ x y)]
                        a (m/return z)
                        b (m/return 4)]
                  (m/return (+ a b)))]
    (t/testing "Can be used to store intermediate results"
      (t/is (= [7]
               (m/mrun program b/sequence-context))))))

(t/deftest guards
  (t/testing "Can be used to filter out values"
    (let [program (m/mdo [x [1 2 3]
                          y [4 5 6]
                          :let [z (+ x y)]
                          :when (even? z)]
                         (m/return z))]
      (t/is (= [6 6 8 8]
               (m/mrun program b/sequence-context))))))
