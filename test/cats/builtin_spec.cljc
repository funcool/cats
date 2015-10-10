(ns cats.builtin-spec
  (:require [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]

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

(t/deftest array-map-monad
  (t/testing "Forms a semigroup"
    (t/is (= (array-map :a 1 :b 2 :c 3 :d 4 :e 5)
             (m/mappend (array-map :a 1 :b 2 :c 3) (array-map :d 4 :e 5)))))

  (t/testing "Forms a monoid"
    (t/is (= (array-map :a 1 :b 2 :c 3 :d 4 :e 5)
             (ctx/with-context b/array-map-context
               (m/mappend (array-map :a 1 :b 2 :c 3 :d 4 :e 5) (m/mempty))))))

  (t/testing "The first monad law: left identity"
    (t/is (= (array-map :aa 1 :bb 2 :cc 3 :dd 4 :ee 5)
             (m/>>= (array-map :a 0 :b 1 :c 2 :d 3 :e 4)
                    (fn [[k v]] (array-map (->> [k k] (map name) (apply str) keyword)
                                           (inc v)))))))

  (t/testing "The second law: right identity"
    (t/is (= (array-map :a 1 :b 2 :c 3 :d 4 :e 5)
             (m/>>= (array-map :a 1 :b 2 :c 3 :d 4 :e 5)
                    m/return))))

  (t/testing "The third law: associativity"
    (t/is (= (m/>>= (m/mlet [x (array-map :a 1 :b 2 :c 3 :d 4 :e 5)
                             y (let [[k v] x] (array-map k (inc v)))]
                      (m/return y))
                    (fn [[k v]] (array-map (str k k) (inc v))))
             (m/>>= (array-map :a 1 :b 2 :c 3 :d 4 :e 5)
                    (fn [x] (m/>>= (let [[k v] x] (array-map k (inc v)))
                                   (fn [[k v]] (array-map (str k k) (inc v))))))))))

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

(t/deftest array-map-foldable
  (t/testing "Foldl"
    (t/is (= (array-map :c 3 :b 2 :a 1)
             (m/foldl (fn [acc [k v]] (into (array-map k v) acc))
                      (array-map)
                      (array-map :a 1 :b 2 :c 3))))
    (t/is (= "a:1;b:2;c:3;d:4;"
             (m/foldl (fn [acc [k v]] (str acc (name k) ":" v ";"))
                      ""
                      (array-map :a 1 :b 2 :c 3 :d 4))))
    (t/is (= 6 (m/foldl (fn [acc [k v]] (+ acc v)) 0 (array-map :a 1 :b 2 :c 3)))))

  (t/testing "Foldr"
    (t/is (= (array-map :a 1 :b 2 :c 3)
             (m/foldr (fn [[k v] acc] (into (array-map k v) acc))
                      (array-map)
                      (array-map :a 1 :b 2 :c 3))))
    (t/is (= "a:1;b:2;c:3;d:4;"
             (m/foldr (fn [[k v] acc] (str (name k) ":" v ";" acc))
                      ""
                      (array-map :a 1 :b 2 :c 3 :d 4))))
    (t/is (= 6 (m/foldr (fn [[k v] acc] (+ acc v)) 0 (array-map :a 1 :b 2 :c 3))))))

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
