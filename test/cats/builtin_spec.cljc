(ns cats.builtin-spec
  (:require [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.data :as d]

            #?(:cljs [cats.context :as ctx :include-macros true]
               :clj  [cats.context :as ctx])

            #?(:cljs [cljs.test :as t]
               :clj  [clojure.test :as t])

            #?(:cljs [cats.core :as m :include-macros true]
               :clj  [cats.core :as m])))

(t/deftest test-nil-as-maybe
  (t/testing "Nil works like nothing (for avoid unnecesary null pointers)."
    (t/is (= (m/>>= nil (fn [_] (m/return 1))) nil))
    (t/is (= (m/fmap inc nil) nil))
    (t/is (maybe/nothing? nil))
    (t/is (maybe/maybe? nil)))

  (t/testing "extract function"
    (t/is (= (m/extract nil) nil))))

(t/deftest map-tests
  (t/testing "Map as monoid"
    (t/is (= (m/mempty b/map-monoid) {})))

  (t/testing "Map as semigroup"
    (t/is (= (m/mappend {:a 1} {:b 2}) {:a 1 :b 2}))
    (t/is (= (m/mappend (m/mempty b/map-monoid) {:a 1}) {:a 1}))))

(t/deftest vector-monad
  (t/testing "Forms a semigroup"
    (t/is (= [1 2 3 4 5]
             (m/mappend [1 2 3] [4 5]))))

  (t/testing "Forms a monoid"
    (t/is (= [1 2 3]
             (ctx/with-context b/vector-context
               (m/mappend [1 2 3] (m/mempty))))))

  (t/testing "The first monad law: left identity"
    (t/is (= [1 2 3 4 5]
             (m/>>= [0 1 2 3 4]
                    (fn [x] [(inc x)])))))

  (t/testing "The second law: right identity"
    (t/is (= [1 2 3]
             (m/>>= [1 2 3]
                    m/return))))

  (t/testing "The third law: associativity"
    (t/is (= (m/>>= (m/mlet [x [1 2 3 4 5]
                             y [(inc x)]]
                      (m/return y))
                    (fn [z] [(inc z)]))
             (m/>>= [1 2 3 4 5]
                    (fn [x] (m/>>= [(inc x)]
                                   (fn [y] [(inc y)]))))))))

(t/deftest sequence-monad
  (let [val->lazyseq (fn [x] (lazy-seq [x]))
        s (val->lazyseq 2)]

    (t/testing "Forms a semigroup"
      (t/is (= [1 2 3 4 5]
               (m/mappend (lazy-seq [1 2 3]) (lazy-seq [4 5])))))

    (t/testing "Forms a monoid"
      (t/is (= [1 2 3]
               (ctx/with-context b/sequence-context
                 (m/mappend (lazy-seq [1 2 3]) (m/mempty))))))

    (t/testing "The first monad law: left identity"
      (t/is (= s (ctx/with-context b/sequence-context
                   (m/>>= (m/return 2)
                          val->lazyseq)))))

    (t/testing "The second monad law: right identity"
      (t/is (= s (m/>>= s m/return))))

    (t/testing "The third monad law: associativity"
      (t/is (= (m/>>= (m/mlet [x s
                               y  (val->lazyseq (inc x))]
                        (m/return y))
                      (fn [y] (val->lazyseq (inc y))))
               (m/>>= s
                      (fn [x]
                        (m/>>= (val->lazyseq (inc x))
                               (fn [y] (val->lazyseq (inc y)))))))))))


(t/deftest set-monad
  (t/testing "Forms a semigroup"
    (t/is (= #{1 2 3 4 5}
             (m/mappend #{1 2 3} #{4 5}))))

  (t/testing "Forms a monoid"
    (t/is (= #{1 2 3}
             (ctx/with-context b/set-context
               (m/mappend #{1 2 3} (m/mempty))))))


  (t/testing "Is a functor"
    (t/is (= #{2 3 4}
             (m/fmap inc #{1 2 3}))))

  (t/testing "The first monad law: left identity"
    (t/is (= #{2} (ctx/with-context b/set-context
                    (m/>>= (m/return 2)
                           (fn [x] #{x}))))))

  (t/testing "The second monad law: right identity"
    (t/is (= #{2} (m/>>= #{2} m/return))))

  (t/testing "The third monad law: associativity"
    (t/is (= (m/>>= (m/mlet [x #{2}
                             y #{(inc x)}]
                      (m/return y))
                    (fn [y] #{(inc y)}))
             (m/>>= #{2}
                    (fn [x]
                      (m/>>= #{(inc x)}
                             (fn [y] #{(inc y)}))))))))


(t/deftest vector-foldable
  (t/testing "Foldl"
    (t/is (= [3 2 1] (m/foldl (fn [acc v] (into [v] acc)) [] [1 2 3])))
    (t/is (= 6 (m/foldl + 0 [1 2 3]))))

  (t/testing "Foldr"
    (t/is (= [1 2 3] (m/foldr (fn [v acc] (into [v] acc)) [] [1 2 3])))
    (t/is (= 6 (m/foldr + 0 [1 2 3])))))

(t/deftest lazyseq-foldable
  (t/testing "Foldl"
    (t/is (= [3 2 1] (m/foldl (fn [acc v] (into [v] acc)) [] (map identity [1 2 3]))))
    (t/is (= 6 (m/foldl + 0 (map identity [1 2 3])))))

  (t/testing "Foldr"
    (t/is (= [1 2 3] (m/foldr (fn [v acc] (into [v] acc)) [] (map identity [1 2 3]))))
    (t/is (= 6 (m/foldr + 0 (map identity [1 2 3]))))))

(t/deftest range-foldable
  (t/testing "Foldl"
    (t/is (= [3 2 1] (m/foldl (fn [acc v] (into [v] acc)) [] (range 1 4))))
    (t/is (= 6 (m/foldl + 0 (range 1 4)))))

  (t/testing "Foldr"
    (t/is (= [1 2 3] (m/foldr (fn [v acc] (into [v] acc)) [] (range 1 4))))
    (t/is (= 6 (m/foldr + 0 (range 1 4))))))

(t/deftest any-monoid
  (t/testing "mempty"
    (ctx/with-context b/any-monoid
      (t/is (= false (m/mempty)))))

  (t/testing "mappend"
    (ctx/with-context b/any-monoid
      (t/is (true? (m/mappend (m/mempty) true)))
      (t/is (false? (m/mappend false false)))
      (t/is (true? (m/mappend true false)))
      (t/is (true? (m/mappend false true)))
      (t/is (true? (m/mappend true true))))))

(t/deftest all-monoid
  (t/testing "mempty"
    (ctx/with-context b/all-monoid
      (t/is (= true (m/mempty)))))

  (t/testing "mappend"
    (ctx/with-context b/all-monoid
      (t/is (false? (m/mappend (m/mempty) false)))
      (t/is (false? (m/mappend false false)))
      (t/is (false? (m/mappend true false)))
      (t/is (false? (m/mappend false true)))
      (t/is (true? (m/mappend true true))))))

(t/deftest sum-monoid
  (t/testing "mempty"
    (ctx/with-context b/sum-monoid
      (t/is (= 0 (m/mempty)))))

  (t/testing "mappend"
    (ctx/with-context b/sum-monoid
      (t/is (= 3 (m/mappend (m/mempty) 3)))
      (t/is (= 3 (m/mappend 1 2))))))

(t/deftest prod-monoid
  (t/testing "mempty"
    (ctx/with-context b/prod-monoid
      (t/is (= 1 (m/mempty)))))

  (t/testing "mappend"
    (ctx/with-context b/prod-monoid
      (t/is (= 6 (m/mappend (m/mempty) 6)))
      (t/is (= 6 (m/mappend 1 2 3)))
      (t/is (= (reduce * (range 1 6)) (apply m/mappend (range 1 6)))))))

(t/deftest string-monoid
  (t/testing "mempty"
    (ctx/with-context b/string-monoid
      (t/is (= "" (m/mempty)))))

  (t/testing "mappend"
    (ctx/with-context b/string-monoid
      (t/is (= "Hello" (m/mappend (m/mempty) "Hello"))))
    (t/is (= "Hello World" (m/mappend "Hello " "World")))
    (t/is (= "abcdefghi" (m/mappend "abc" "def" "ghi")))))

(t/deftest pair-monoid
  (t/testing "mempty"
    (ctx/with-context (b/pair-monoid b/string-monoid)
      (t/is (= (d/pair "" "") (m/mempty))))

    (ctx/with-context (b/pair-monoid b/sum-monoid)
      (t/is (= (d/pair 0 0) (m/mempty)))))

  (t/testing "mappend"
    (t/is (= (d/pair "Hello buddy" "Hello mate")
             (m/mappend
              (d/pair "Hello " "Hello ")
              (d/pair "buddy" "mate")))))

  (t/testing "mappend with other-context"
    (ctx/with-context (b/pair-monoid b/sum-monoid)
      (t/is (= (d/pair 10 20)
               (m/mappend
                (d/pair 3 5)
                (d/pair 3 5)
                (d/pair 4 10)))))))

(defn inc-if-even
  [n]
  (if (even? n)
    (maybe/just (inc n))
    (maybe/nothing)))

(t/deftest vector-traversable
  (t/testing "Traverse"
    (t/is (= (maybe/just [])
             (ctx/with-context maybe/context
               (m/traverse inc-if-even []))))
    (t/is (= (maybe/just [3 5])
             (ctx/with-context maybe/context
               (m/traverse inc-if-even [2 4]))))
    (t/is (= (maybe/nothing)
             (ctx/with-context maybe/context
               (m/traverse inc-if-even [1 2]))))))

(t/deftest lazyseq-traversable
  (t/testing "Traverse"
    (t/is (= (maybe/just [])
             (ctx/with-context maybe/context
               (m/traverse inc-if-even (lazy-seq [])))))
    #_(t/is (= (maybe/just [3 5])
             (ctx/with-context maybe/context
               (m/traverse inc-if-even (lazy-seq [2 4])))))
    #_(t/is (= (maybe/nothing)
             (ctx/with-context maybe/context
               (m/traverse inc-if-even (lazy-seq [1 2])))))))
