(ns cats.labs.manifold-spec
  (:require [manifold.deferred :as d]
            [clojure.test :as t]
            [cats.builtin :as b]
            [cats.context :as ctx]
            [cats.core :as m]
            [cats.labs.manifold :as mf]
            [cats.monad.either :as either]))

(t/deftest deferred-as-functor
  (let [ch (m/pure mf/deferred-context 1)]
    (t/is (= 2 @(m/fmap inc ch)))))

(t/deftest deferred-as-monad-1
  (let [ch (m/pure mf/deferred-context 1)]
    (t/is (= 2 @(m/>>= ch (fn [x] (m/return (inc x))))))))

(t/deftest deferred-as-monad-2
  (let [ch1 (d/future 1)
        ch2 (d/future 1)
        ch3 (d/future 1)
        r   (m/mlet [x ch1
                     y ch2
                     z ch3]
              (m/return (+ x y z)))]
    (t/is (= 3 @r))))

(t/deftest first-monad-law-left-identity
  (let [ch1 (m/pure mf/deferred-context 4)
        ch2 (m/pure mf/deferred-context 4)
        vl  (m/>>= ch2 #(m/pure mf/deferred-context %))]
    (t/is (= @ch1 @vl))))

(t/deftest second-monad-law-right-identity
  (let [ch1 (d/future 3)
        rs  (m/>>= (d/future 3) m/return)]
    (t/is (= @ch1 @rs))))

(t/deftest third-monad-law-associativity
  (let [rs1 (m/>>= (m/mlet [x (d/future 2)
                            y (d/future (inc x))]
                     (m/return y))
                   (fn [y] (d/future (inc y))))
        rs2 (m/>>= (d/future 2)
                   (fn [x] (m/>>= (d/future (inc x))
                                  (fn [y] (d/future (inc y))))))]
    (t/is (= @rs1 @rs2))))

(t/deftest semigroup-tests
  (let [c1 (d/success-deferred {:a 1})
        c2 (d/success-deferred {:b 2})
        r (m/mappend c1 c2)]
    (t/is (= {:a 1 :b 2} @r)))
  (let [c1 (d/success-deferred {:a 1})
        c2 (d/error-deferred {:b 2})
        r (m/mappend c1 c2)]
    (t/is (thrown? clojure.lang.ExceptionInfo @r))))

(t/deftest applicative-do
  (letfn [(async-call [wait]
            (d/future
              (Thread/sleep 100)
              wait))]
    (let [result (m/alet [x (async-call 100)
                          y (async-call 100)]
                         (+ x y))]
      (t/is (d/deferred? result))
      (t/is (= @result 200)))))

(def deferred-either-m (either/either-t mf/deferred-context))

(t/deftest deferred-transformer-tests
  (t/testing "deferred combination with either"
    (let [funcright #(d/future (either/right %))
          funcleft #(d/future (either/left %))

          r1 (ctx/with-context deferred-either-m
               (m/mlet [x (funcright 1)
                        y (funcright 2)]
                 (m/return (+ x y))))

          r2 (ctx/with-context deferred-either-m
               (m/mlet [x (funcright 1)
                        y (funcleft :foo)
                        z (funcright 2)]
                 (m/return (+ x y))))]

      (t/is (= (either/right 3) @r1))
      (t/is (= (either/left :foo) @r2)))))
