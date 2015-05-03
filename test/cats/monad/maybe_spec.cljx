(ns cats.monad.maybe-spec
  #+cljs
  (:require [cljs.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [clojure.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.core :as m]))

(t/deftest maybe-monad-tests
  (t/testing "Basic maybe operations."
    (t/is (= 1 (maybe/from-maybe (maybe/just 1))))
    (t/is (= 1 (maybe/from-maybe (maybe/just 1) 42)))
    (t/is (= nil (maybe/from-maybe (maybe/nothing))))
    (t/is (= 42 (maybe/from-maybe (maybe/nothing) 42))))

  (t/testing "extract function"
    (t/is (= (p/extract (maybe/just 1)) 1))
    (t/is (= (p/extract (maybe/nothing)) nil)))

  (t/testing "Test IDeref"
    (t/is (= nil @(maybe/nothing)))
    (t/is (= 1 @(maybe/just 1))))

  (t/testing "Test predicates"
    (let [m1 (maybe/just 1)]
      (t/is (maybe/maybe? m1))
      (t/is (maybe/just? m1))))

  (t/testing "Test fmap"
    (let [m1 (maybe/just 1)
          m2 (maybe/nothing)]
      (t/is (= (m/fmap inc m1) (maybe/just 2)))
      (t/is (= (m/fmap inc m2) (maybe/nothing)))))

  (t/testing "Forms a Semigroup when its values form a Semigroup"
    (t/is (= (maybe/just [1 2 3 4 5])
             (m/mappend (maybe/just [1 2 3]) (maybe/just [4 5])))))

  (t/testing "Its identity element is Nothing"
    (t/is (= (maybe/nothing)
             (m/with-monad maybe/maybe-monad
               (m/mempty)))))

  (t/testing "The first monad law: left identity"
    (t/is (= (maybe/just 2)
             (m/>>= (p/mreturn maybe/maybe-monad 2) maybe/just))))

  (t/testing "The second monad law: right identity"
    (t/is (= (maybe/just 2)
             (m/>>= (maybe/just 2) m/return))))

  (t/testing "The third monad law: associativity"
    (t/is (= (m/>>= (m/mlet [x  (maybe/just 2)
                             y  (maybe/just (inc x))]
                      (m/return y))
                    (fn [y] (maybe/just (inc y))))
             (m/>>= (maybe/just 2)
                    (fn [x] (m/>>= (maybe/just (inc x))
                                   (fn [y] (maybe/just (inc y))))))))))

(def maybe-vector-transformer (maybe/maybe-transformer b/vector-monad))

(t/deftest maybe-transformer-tests
  (t/testing "It can be combined with the effects of other monads"
    (t/is (= [(maybe/just 2)]
             (m/with-monad maybe-vector-transformer
               (m/return 2))))

    (t/is (= [(maybe/just 1)
              (maybe/just 2)
              (maybe/just 2)
              (maybe/just 3)]
             (m/with-monad maybe-vector-transformer
               (m/mlet [x [(maybe/just 0) (maybe/just 1)]
                        y [(maybe/just 1) (maybe/just 2)]]
                 (m/return (+ x y))))))

    (t/is (= [(maybe/just 1)
              (maybe/just 2)
              (maybe/just 2)
              (maybe/just 3)]
             (m/with-monad maybe-vector-transformer
               (m/mlet [x (m/lift [0 1])
                        y (m/lift [1 2])]
                 (m/return (+ x y))))))

    (t/is (= [(maybe/just 1)
              (maybe/nothing)
              (maybe/just 2)
              (maybe/nothing)]
             (m/with-monad maybe-vector-transformer
               (m/mlet [x [(maybe/just 0) (maybe/just 1)]
                        y [(maybe/just 1) (maybe/nothing)]]
                 (m/return (+ x y))))))))

(t/deftest maybe-test
  (let [n (maybe/nothing)
        j (maybe/just 42)]
    (t/is (= 42 (maybe/maybe 42 n inc)))
    (t/is (= 43 (maybe/maybe 42 j inc)))))

(t/deftest seq-conversion-test
  (let [n (maybe/nothing)
        j (maybe/just 42)]
    (t/is (= n (maybe/seq->maybe [])))
    (t/is (= j (maybe/seq->maybe [42 99])))
    (t/is (= [] (maybe/maybe->seq n)))
    (t/is (= [42] (maybe/maybe->seq j)))))

(t/deftest cat-maybes-test
  (let [n1 (maybe/nothing)
        n2 (maybe/nothing)
        j1 (maybe/just 42)
        j2 (maybe/just 99)
        ms [n1 n2 j1 j2]]
    (t/is (= [42 99] (maybe/cat-maybes ms)))))

(t/deftest map-maybe-test
  (let [just-evens #(if (even? %) (maybe/just %) (maybe/nothing))]
    (t/is (= [42 100] (maybe/map-maybe just-evens [41 42 99 100])))))
