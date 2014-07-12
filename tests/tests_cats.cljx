(ns tests-cats
  #+cljs
  (:require-macros [cemerick.cljs.test
                    :refer (is deftest with-test run-tests testing test-var)]
                   [cats.core :refer (mlet with-context lift-m)])
  #+cljs
  (:require [cemerick.cljs.test :as ts]
            [cats.core :as m]
            [cats.protocols :as p]
            [cats.monad.maybe :as t]
            [cats.monad.continuation :as cont]
            [cats.monad.state :as state])
  #+clj
  (:require [clojure.test :refer :all]
            [cats.core :as m :refer [mlet with-context lift-m]]
            [cats.types :as t]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.monad.continuation :as cont]
            [cats.monad.state :as state]))


(deftest common
  (testing "Basic maybe operations."
    (is (= 1 (maybe/from-maybe (maybe/just 1))))
    (is (= nil (maybe/from-maybe (maybe/nothing))))))

(deftest test-mlet
  (testing "It supports regular let inside its bindings")
    (is (= (maybe/just 2)
           (mlet [i (maybe/just 1)
                  :let [i (inc i)]]
                 (m/return i))))
  (testing "It supports :when guards inside its bindings")
    (is (= (maybe/nothing)
           (mlet [i (maybe/just 2)
                  :when (> i 2)]
                 (m/return i))))
    (is (= [3 4 5]
           (mlet [i [1 2 3 4 5]
                  :when (> i 2)]
                 (m/return i))))
  (testing "The body runs in an implicit do"
    (is (= (maybe/just 3)
           (mlet [i (maybe/just 2)
                  :let [x (inc i)]]
                 (assert (= x 3))
                 (m/return x))))))

(deftest test-sequence
  (testing "It works with vectors"
    (is (= (m/sequence [[1 2] [3 4]])
           [[1 3] [1 4] [2 3] [2 4]])))
;  (testing "It works with lazy seqs"
;    (is (= (m/sequence [(lazy-seq [1 2]) (lazy-seq [3 4])])
;           '([1 3] [1 4] [2 3] [2 4]))))
;  (testing "It works with sets"
;    (is (= (m/sequence [#{1 2} #{3 4}])
;           #{[1 3] [1 4] [2 3] [2 4]})))
  (testing "It works with Maybe values"
    (is (= (m/sequence [(maybe/just 2) (maybe/just 3)])
           (maybe/just [2 3])))
    (is (= (m/sequence [(maybe/just 2) (maybe/nothing)])
           (maybe/nothing)))))
;
(deftest test-mapseq
  (testing "It works with maybe values"
    (is (= (m/mapseq maybe/just [1 2 3 4 5])
           (maybe/just [1 2 3 4 5])))
    (is (= (maybe/nothing)
           (m/mapseq (fn [v]
                        (if (odd? v)
                          (maybe/just v)
                          (maybe/nothing)))
                      [1 2 3 4 5])))))

(deftest test-lift-m
  (let [monad+ (lift-m 2 +)]
    (testing "It can lift a function to the vector monad"
      (is (= [1 2 3 4 5 6]
             (monad+ [0 2 4] [1 2]))))
    (testing "It can lift a function to the Maybe monad"
      (is (= (maybe/just 6)
             (monad+ (maybe/just 2) (maybe/just 4))))
      (is (= (maybe/nothing)
             (monad+ (maybe/just 1) (maybe/nothing)))))))

(deftest test-filter
  (testing "It can filter Maybe monadic values"
    (let [bigger-than-4 (partial < 4)]
      (is (= (maybe/just 6)
             (m/filter bigger-than-4 (maybe/just 6))))
      (is (= (maybe/nothing)
             (m/filter bigger-than-4 (maybe/just 3))))))
  (testing "It can filter vectors"
    (is (= [1 3 5]
           (m/filter odd? [1 2 3 4 5 6])))))

(deftest test-when
  (testing "It returns the monadic value unchanged when the condition is true"
    (is (= (maybe/just 3)
           (m/when true (maybe/just 3)))))
  (testing "It returns nil in the monadic context when the condition is false"
    (is (= [nil]
           (m/when false [])))))

(deftest test-maybe
  (testing "Test predicates"
    (let [m1 (maybe/just 1)]
      (is (maybe/maybe? m1))
      (is (maybe/just? m1))))

  (testing "Test fmap"
    (let [m1 (maybe/just 1)
          m2 (maybe/nothing)]
      (is (= (m/fmap inc m1) (maybe/just 2)))
      (is (= (m/fmap inc m2) (maybe/nothing)))))

  (testing "The first monad law: left identity"
    (is (= (maybe/just 2)
           (m/>>= (p/mreturn maybe/maybe-monad 2) maybe/just))))

  (testing "The second monad law: right identity"
    (is (= (maybe/just 2)
           (m/>>= (maybe/just 2) m/return))))

  (testing "The third monad law: associativity"
    (is (= (m/>>= (mlet [x  (maybe/just 2)
                         y  (maybe/just (inc x))]
                        (m/return y))
                  (fn [y] (maybe/just (inc y))))
           (m/>>= (maybe/just 2)
                  (fn [x] (m/>>= (maybe/just (inc x))
                                (fn [y] (maybe/just (inc y))))))))))

; TODO: test maybe transformer

(deftest test-continuation-monad
  (let [cont-42 (cont/continuation (fn [c] (c 42)))
        inc-cont-fn (fn [x]
                      (cont/continuation (fn [c] (c (inc x)))))]

    (testing "The first monad law: left identity"
      (is (= (cont/run-cont cont-42)
             (cont/run-cont
               (with-context cont/continuation-monad
                 (m/>>= (m/return 42)
                        (fn [v] (cont/continuation (fn [c] c v)))))))))

    (testing "The second monad law: right identity"
      (is (= (cont/run-cont cont-42)
             (cont/run-cont
               (m/>>= cont-42 m/return)))))

    (testing "The third monad law: associativity"
      (is (= (m/>>= (mlet [x  cont-42
                           y  inc-cont-fn]
                         (m/return y))
                         inc-cont-fn))
             (m/>>= cont-42
                    (fn [x] (m/>>= (cont/continuation (fn [c] (c (inc x))))
                                   inc-cont-fn)))))

    (testing "call-cc allows the creation of resumable computations."
      (let [cc (atom nil)]
        (is (= 44
               (cont/run-cont (mlet [x cont-42
                                  y (cont/call-cc (fn [k]
                                                    (reset! cc k)
                                                    (k 2)))]
                                 (m/return (+ x y))))))
        (is (= 45
               (cont/run-cont (@cc 3))))
        (is (= 46
               (cont/run-cont (@cc 4))))))))

;(deftest test-lazy-seq
;  (let [s (lazy-seq [2])
;        val->lazyseq (fn [x] (lazy-seq [x]))]
;    (testing "The first monad law: left identity"
;      (is (= s
;             (with-context s
;               (m/>>= (m/return 2)
;                      val->lazyseq)))))
;
;    (testing "The second monad law: right identity"
;      (is (= s
;             (m/>>= s
;                    m/return))))
;
;    (testing "The third monad law: associativity"
;      (is (= (m/>>= (mlet [x  s
;                           y  (val->lazyseq (inc x))]
;                          (m/return y))
;                    (fn [y] (val->lazyseq (inc y))))
;             (m/>>= s
;                    (fn [x] (m/>>= (val->lazyseq (inc x))
;                                  (fn [y] (val->lazyseq (inc y)))))))))))

(deftest test-vector
  (testing "The first monad law: left identity"
    (is (= [1 2 3 4 5]
           (m/>>= [0 1 2 3 4]
                  (fn [x] [(inc x)])))))
  (testing "The second law: right identity"
    (is (= [1 2 3]
           (m/>>= [1 2 3]
                  m/return))))
  (testing "The third law: associativity"
    (is (= (m/>>= (mlet [x [1 2 3 4 5]
                         y [(inc x)]]
                        (m/return y))
                  (fn [z] [(inc z)]))

           (m/>>= [1 2 3 4 5]
                  (fn [x] (m/>>= [(inc x)]
                                (fn [y] [(inc y)]))))))))

(deftest state-monad
  (testing "get-state should return the identity."
    (let [computation (state/get-state)]
      (is (= :foo (state/exec-state computation :foo)))))

  (testing "swap-state should should apply function to state and return it."
    (let [computation (state/swap-state inc)]
      (is (= 2 (state/exec-state computation 1)))))

  (testing "State monad compositio with mlet should return state"
    (let [res (mlet [s (state/get-state)]
                (m/return (inc s)))]
      (is (state/state? res))
      (let [res (state/run-state res 1)]
        (is (state/pair? res))
        (is (= 2 (first res)))
        (is (= 1 (second res)))))))
