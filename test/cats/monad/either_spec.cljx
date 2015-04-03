(ns cats.monad.either-spec
  #+cljs
  (:require [cljs.test :as t]
            [clojure.string :as s]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.either :as either]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [clojure.test :as t]
            [clojure.string :as s]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.either :as either]
            [cats.core :as m]))

(t/deftest basic-operations-test
  (t/is (= 1 (either/from-either (either/right 1))))
  (t/is (= nil (either/from-either (either/left)))))

(t/deftest ideref-test
  (t/is (= 1 @(either/left 1)))
  (t/is (= 1 @(either/right 1))))

(t/deftest predicates-test
  (let [m1 (either/right 1)
        m2 (either/left 42)]
    (t/is (either/either? m1))
    (t/is (either/either? m2))
    (t/is (either/right? m1))
    (t/is (either/left? m2))))

(t/deftest functor-test
  (let [m1 (either/right 1)
        m2 (either/left)]
    (t/is (= (m/fmap inc m1) (either/right 2)))
    (t/is (= (m/fmap inc m2) (either/left)))))

(t/deftest first-monad-law-left-identity
  (t/is (= (either/right 2)
           (m/>>= (p/mreturn either/either-monad 2) either/right))))

(t/deftest second-monad-law-right-identity
  (t/is (= (either/right 2)
           (m/>>= (either/right 2) m/return))))

(t/deftest third-monad-law-associativity
  (t/is (= (m/>>= (m/mlet [x  (either/right 2)
                           y  (either/right (inc x))]
                    (m/return y))
                  (fn [y] (either/right (inc y))))
           (m/>>= (either/right 2)
                  (fn [x] (m/>>= (either/right (inc x))
                                 (fn [y] (either/right (inc y)))))))))


(def either-vector-m
  (either/either-transformer b/vector-monad))

(t/deftest either-transformer-tests
  (t/is (= [(either/right 2)]
           (m/with-monad either-vector-m
             (m/return 2))))

  (t/is (= [(either/right 1)
            (either/right 2)
            (either/right 2)
            (either/right 3)]
           (m/with-monad either-vector-m
             (m/mlet [x [(either/right 0) (either/right 1)]
                      y [(either/right 1) (either/right 2)]]
               (m/return (+ x y))))))

  (t/is (= [(either/right 1)
            (either/right 2)
            (either/right 2)
            (either/right 3)]
           (m/with-monad either-vector-m
             (m/mlet [x (m/lift [0 1])
                      y (m/lift [1 2])]
               (m/return (+ x y))))))

  (t/is (= [(either/right 1)
            (either/left)
            (either/right 2)
            (either/left)]
           (m/with-monad either-vector-m
             (m/mlet [x [(either/right 0) (either/right 1)]
                      y [(either/right 1) (either/left)]]
               (m/return (+ x y)))))))

(t/deftest branch-test
  (let [l (either/left "oh no")
        r (either/right 42)]
    (t/is (= "OH NO" (either/branch l s/upper-case inc)))
    (t/is (= 43 (either/branch r s/upper-case inc)))

))
