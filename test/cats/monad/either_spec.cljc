(ns cats.monad.either-spec
  #?(:cljs
     (:require [cljs.test :as t]
               [clojure.string :as s]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.monad.either :as either]
               [cats.context :as ctx :include-macros true]
               [cats.core :as m :include-macros true])
     :clj
     (:require [clojure.test :as t]
               [clojure.string :as s]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.monad.either :as either]
               [cats.context :as ctx]
               [cats.core :as m])))

(t/deftest basic-operations-test
  (t/is (= 1 (m/extract (either/right 1))))
  (t/is (= nil (m/extract (either/left)))))

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
           (m/>>= (m/return either/context 2) either/right))))

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
  (either/either-transformer b/vector-context))

(t/deftest either-transformer-tests
  (t/is (= [(either/right 2)]
           (ctx/with-context either-vector-m
             (m/return 2))))

  (t/is (= [(either/right 1)
            (either/right 2)
            (either/right 2)
            (either/right 3)]
           (ctx/with-context either-vector-m
             (m/mlet [x [(either/right 0) (either/right 1)]
                      y [(either/right 1) (either/right 2)]]
               (m/return (+ x y))))))

  (t/is (= [(either/right 1)
            (either/right 2)
            (either/right 2)
            (either/right 3)]
           (ctx/with-context either-vector-m
             (m/mlet [x (m/lift [0 1])
                      y (m/lift [1 2])]
               (m/return (+ x y))))))

  (t/is (= [(either/right 1)
            (either/left)
            (either/right 2)
            (either/left)]
           (ctx/with-context either-vector-m
             (m/mlet [x [(either/right 0) (either/right 1)]
                      y [(either/right 1) (either/left)]]
               (m/return (+ x y)))))))

(t/deftest branch-test
  (let [l (either/left "oh no")
        r (either/right 42)]
    (t/is (= "OH NO" (either/branch l s/upper-case inc)))
    (t/is (= 43 (either/branch r s/upper-case inc)))))

(t/deftest branch-left-test
  (let [l (either/left "oh no")
        r (either/right 42)]
    (t/is (= "OH NO" (either/branch-left l s/upper-case)))
    (t/is (= r (either/branch-left r s/upper-case)))))

(t/deftest branch-right-test
  (let [l (either/left "oh no")
        r (either/right 42)]
    (t/is (= l (either/branch-right l inc)))
    (t/is (= 43 (either/branch-right r inc)))))


(t/deftest filtering-test
  (let [l1 (either/left "oh no")
        l2 (either/left "yo ho ho ho")
        r1 (either/right 42)
        r2 (either/right 99)
        es [l1 l2 r1 r2]]
    (t/is (every? either/left? (either/lefts es)))
    (t/is (every? either/right? (either/rights es)))
    (t/is (= l1 (either/first-left es)))
    (t/is (= r1 (either/first-right es)))))

(t/deftest invert-test
  (let [l (either/left "oh no")
        r (either/right "oh no")]
    (t/is (= r (either/invert l)))
    (t/is (= l (either/invert r)))))

(t/deftest foldable-test
  (t/testing "Foldl"
    (t/is (= (either/right 2)
             (m/foldl #(m/return (+ %1 %2)) 1 (either/right 1))))
    (t/is (= 1
             (m/foldl #(m/return (+ %1 %2)) 1 (either/left 5)))))

  (t/testing "Foldr"
    (t/is (= (either/right 2)
             (m/foldr #(m/return (+ %1 %2)) 1 (either/right 1))))
    (t/is (= 1
             (m/foldr #(m/return (+ %1 %2)) 1 (either/left 5))))))
