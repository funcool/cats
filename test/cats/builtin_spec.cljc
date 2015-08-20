(ns cats.builtin-spec
  (:require [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.context :as ctx]
            [cats.data :as d]

            #?(:cljs [cljs.test :as t]
               :clj [clojure.test :as t])

            #?(:cljs [cats.core :as m :include-macros true]
               :clj [cats.core :as m])))

(t/deftest test-nil-as-maybe
  (t/testing "Nil works like nothing (for avoid unnecesary null pointers)."
    (t/is (= (m/>>= nil (fn [_] (m/return 1))) nil))
    (t/is (= (m/fmap inc nil) nil))
    (t/is (maybe/nothing? nil))
    (t/is (maybe/maybe? nil)))

  (t/testing "extract function"
    (t/is (= (p/extract nil) nil))))

(t/deftest vector-monad
  (t/testing "Forms a semigroup"
    (t/is (= [1 2 3 4 5]
             (m/mappend [1 2 3] [4 5]))))

  (t/testing "Forms a monoid"
    (t/is (= [1 2 3]
             (m/with-monad b/vector-monad
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
               (m/with-monad b/sequence-monad
                 (m/mappend (lazy-seq [1 2 3]) (m/mempty))))))

    (t/testing "The first monad law: left identity"
      (t/is (= s (m/with-monad b/sequence-monad
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
             (m/with-monad b/set-monad
               (m/mappend #{1 2 3} (m/mempty))))))


  (t/testing "Is a functor"
    (t/is (= #{2 3 4}
             (m/fmap inc #{1 2 3}))))

  (t/testing "The first monad law: left identity"
    (t/is (= #{2} (m/with-monad b/set-monad
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
    (t/is (= [2 3 4] (m/foldl #(conj %1 (inc %2)) [] [1 2 3])))
    (t/is (= 6 (m/foldl + 0 [1 2 3]))))

  (t/testing "Foldr"
    (t/is (= [2 3 4] (m/foldr #(cons (inc %1) %2) [] [1 2 3])))
    (t/is (= 6 (m/foldr + 0 [1 2 3])))))

(t/deftest lazyseq-foldable
  (letfn [(foldl-fn [state acc x]
            (swap! state inc)
            (conj acc (inc x)))
          (foldr-fn [state x acc]
            (swap! state inc)
            (cons (inc x) acc))]
    (t/testing "Foldl"
      (let [state (atom 0)
            result (m/foldl (partial foldl-fn state) [] (map identity [1 2 3 4]))]
        (t/is (= @state 0))
        (t/is (= 2 (first result)))
        (t/is (= @state 4))))
    (t/testing "Foldr"
      (let [state (atom 0)
            result (m/foldr (partial foldr-fn state) '() (map identity [1 2 3 4]))]
        (t/is (= @state 0))
        (t/is (= 2 (first result)))
        (t/is (= @state 1))))))

(t/deftest any-monoid
  (t/testing "mempty"
    (ctx/with-context b/any-monoid
      (t/is (= false (p/mempty (ctx/get-current))))))
  (t/testing "mappend"
    (ctx/with-context b/any-monoid
      (t/is (false? (m/mappend false false)))
      (t/is (true? (m/mappend true false)))
      (t/is (true? (m/mappend false true)))
      (t/is (true? (m/mappend true true))))))

(t/deftest all-monoid
  (t/testing "mempty"
    (ctx/with-context b/all-monoid
      (t/is (= true (p/mempty (ctx/get-current))))))
  (t/testing "mappend"
    (ctx/with-context b/all-monoid
      (t/is (false? (m/mappend false false)))
      (t/is (false? (m/mappend true false)))
      (t/is (false? (m/mappend false true)))
      (t/is (true? (m/mappend true true))))))

(t/deftest sum-monoid
  (t/testing "mempty"
    (ctx/with-context b/sum-monoid
      (t/is (= 0 (p/mempty (ctx/get-current))))))
  (t/testing "mappend"
    (ctx/with-context b/sum-monoid
      (t/is (= 3 (m/mappend 1 2))))))

(t/deftest prod-monoid
  (t/testing "mempty"
    (ctx/with-context b/prod-monoid
      (t/is (= 1 (p/mempty (ctx/get-current))))))
  (t/testing "mappend"
    (ctx/with-context b/prod-monoid
      (t/is (= 6 (m/mappend 1 2 3)))
      (t/is (= (reduce * (range 1 6)) (apply m/mappend (range 1 6)))))))

(t/deftest string-monoid
  (t/testing "mempty"
    (ctx/with-context b/string-monoid
      (t/is (= "" (p/mempty (ctx/get-current))))))
  (t/testing "mappend"
    (t/is (= "Hello World" (m/mappend "Hello " "World")))
    (t/is (= "abcdefghi" (m/mappend "abc" "def" "ghi")))))

(t/deftest pair-monoid
  (t/testing "mempty"
    (ctx/with-context b/string-monoid
      (t/is (= (d/pair "" "") (p/mempty b/pair-monoid))))
    (ctx/with-context b/sum-monoid
      (t/is (= (d/pair 0 0) (p/mempty b/pair-monoid)))))
  (t/testing "mappend"
    (t/is (= (d/pair "Hello buddy" "Hello mate")
             (m/mappend
              (d/pair "Hello " "Hello ")
              (d/pair "buddy" "mate"))))
    ;; This won't work due to explicitly set context
    ;; (ctx/with-context b/sum-monoid
    ;;   (t/is (= (d/pair 10 20)
    ;;            (m/mappend
    ;;             (d/pair 3 5)
    ;;             (d/pair 3 5)
    ;;             (d/pair 4 10)))))
    ))
