(ns cats.core-spec
  #?(:cljs
     (:require [cljs.test :as t]
               [cats.builtin :as b]
               [cats.monad.maybe :as maybe]
               [cats.core :as m :include-macros true])
     :clj
     (:require [clojure.test :as t]
               [cats.builtin :as b]
               [cats.monad.maybe :as maybe]
               [cats.core :as m])))

(defn add2 [x y]
  (+ x y))

(t/deftest fapply-test
  (t/testing "Simple fapply run."
    (t/is (= 2 @(m/fapply (maybe/just inc) (maybe/just 1)))))
  (t/testing "Variadic fapply run."
    (t/is (= 3 @(m/fapply (maybe/just #(partial + %))
                          (maybe/just 1)
                          (maybe/just 2))))))

(t/deftest mlet-tests
  (t/testing "Support regular let bindings inside mlet"
    (t/is (= (maybe/just 2)
             (m/mlet [i (maybe/just 1)
                      :let [i (inc i)]]
               (m/return i)))))

  (t/testing "Support :when guards inside its bindings"
    (t/is (= (maybe/nothing)
             (m/mlet [i (maybe/just 2)
                      :when (> i 2)]
               (m/return i))))
    (t/is (= [3 4 5]
             (m/mlet [i [1 2 3 4 5]
                      :when (> i 2)]
               (m/return i)))))

  (t/testing "The body runs in an implicit do"
    (t/is (= (maybe/just 3)
             (m/mlet [i (maybe/just 2)
                      :let [x (inc i)]]
               (assert (= x 3))
               (m/return x))))))

(t/deftest alet-tests
  (t/testing "It works with just one applicative binding"
    (t/is (= (maybe/just 3)
             (m/alet [x (maybe/just 2)]
                     (inc x)))))

  (t/testing "It works with no dependencies between applicative values"
    (t/is (= (maybe/just 3)
             (m/alet [x (maybe/just 1)
                      y (maybe/just 2)]
                     (add2 x y)))))

  (t/testing "It works with one level of dependencies between applicative values"
    (t/is (= (maybe/just [42])
             (m/alet [x (maybe/just 21)       ;; split 1
                      y (maybe/just 2)
                      z (maybe/just (* x y))] ;; split 2
                     (vector z)))))

  (t/testing "It works with more than one level of dependencies between applicative values"
    (t/is (= (maybe/just [45])
             (m/alet [x (maybe/just 21)       ;; split 1
                      y (maybe/just 2)
                      z (maybe/just (* x y))  ;; split 2
                      z (maybe/just (+ 3 z))] ;; split 3
                     (vector z)))))

  (t/testing "It works with more than one level of dependencies, with distinct split sizes"
    (t/is (= (maybe/just 66)
             (m/alet [x (maybe/just 21)         ;; split 1
                      y (maybe/just 2)
                      z (maybe/just (* x y))    ;; split 2
                      a (maybe/just (* 3 x))
                      b (maybe/just 1)          ;; split 3
                      c (maybe/just 2)
                      d (maybe/just (+ a b c))] ;; split 4
               d))))

  (t/testing "It renames the body symbols correctly"
    (t/is (= (maybe/just 42)
             (m/alet [x (maybe/just 5)
                      y (maybe/just 6)
                      x (maybe/just (inc x))
                      y (maybe/just (inc y))]
               (* x y))))))

(t/deftest sequence-tests
  (t/testing "It works with vectors"
    (t/is (= (m/sequence [[1 2] [3 4]])
             [[1 3] [1 4] [2 3] [2 4]])))

  (t/testing "It works with lazy seqs"
    (t/is (= (m/sequence [(lazy-seq [1 2]) (lazy-seq [3 4])])
             [[1 3] [1 4] [2 3] [2 4]])))

  (t/testing "It works with sets"
    (t/is (= (m/sequence [#{1 2} #{3 4}])
             #{[1 3] [1 4] [2 3] [2 4]})))

  (t/testing "It works with Maybe values"
    (t/is (= (maybe/just [2 3])
             (m/sequence [(maybe/just 2) (maybe/just 3)])))

    (t/is (= (maybe/nothing)
             (m/sequence [(maybe/just 2) (maybe/nothing)])))))


(t/deftest mapseq-tests
  (t/testing "It works with Maybe values"
    (t/is (= (m/mapseq maybe/just [1 2 3 4 5])
             (maybe/just [1 2 3 4 5])))
    (t/is (= (maybe/nothing)
             (m/mapseq (fn [v]
                         (if (odd? v)
                           (maybe/just v)
                           (maybe/nothing)))
                       [1 2 3 4 5])))))
(t/deftest lift-m-tests
  (let [monad+ (m/lift-m 2 +)]
    (t/testing "It can lift a function to the vector monad"
      (t/is (= [1 2 3 4 5 6]
               (monad+ [0 2 4] [1 2]))))

    (t/testing "It can lift a function to the Maybe monad"
      (t/is (= (maybe/just 6)
               (monad+ (maybe/just 2) (maybe/just 4))))
      (t/is (= (maybe/nothing)
               (monad+ (maybe/just 1) (maybe/nothing)))))

    (t/testing "It can lift a function to a Monad Transformer"
      (let [maybe-sequence-monad (maybe/maybe-transformer b/sequence-monad)]
        (t/is (= [(maybe/just 1) (maybe/just 2)
                  (maybe/just 3) (maybe/just 4)
                  (maybe/just 5) (maybe/just 6)]
                 (m/with-monad maybe-sequence-monad
                   (monad+ [(maybe/just 0) (maybe/just 2) (maybe/just 4)]
                           [(maybe/just 1) (maybe/just 2)]))))))))

(t/deftest fixed-arity-lift-m-tests
  #?(:clj
     (let [monad+ (m/lift-m add2)]
       (t/testing "It can lift a function to the vector monad"
         (t/is (= [1 2 3 4 5 6]
                  (monad+ [0 2 4] [1 2]))))))

  (t/testing "It can lift a function to the Maybe monad"
    (let [monad+ (m/lift-m 2 add2)]
      (t/is (= (maybe/just 6)
               (monad+ (maybe/just 2) (maybe/just 4))))
      (t/is (= (maybe/nothing)
               (monad+ (maybe/just 1) (maybe/nothing))))))

  (t/testing "Currying and lifting can be combined"
    (let [curry-monad+ (m/curry-lift-m 2 add2)]
      (t/is (= (maybe/just 6)
               ((curry-monad+ (maybe/just 1)) (maybe/just 5))))))

  (t/testing "It can lift a function to a Monad Transformer"
    (let [maybe-sequence-monad (maybe/maybe-transformer b/sequence-monad)
          monad+ (m/lift-m 2 add2)]
      (t/is (= [(maybe/just 1) (maybe/just 2)
                (maybe/just 3) (maybe/just 4)
                (maybe/just 5) (maybe/just 6)]
               (m/with-monad maybe-sequence-monad
                 (monad+ [(maybe/just 0) (maybe/just 2) (maybe/just 4)]
                         [(maybe/just 1) (maybe/just 2)])))))))

(t/deftest filter-tests
  (t/testing "It can filter Maybe monadic values"
    (let [bigger-than-4 (partial < 4)]
      (t/is (= (maybe/just 6)
               (m/filter bigger-than-4 (maybe/just 6))))
      (t/is (= (maybe/nothing)
               (m/filter bigger-than-4 (maybe/just 3))))))
  (t/testing "It can filter vectors"
    (t/is (= [1 3 5]
             (m/filter odd? [1 2 3 4 5 6])))))

(t/deftest when-tests
  (t/testing "It returns the monadic value unchanged when the condition is true"
    (t/is (= (maybe/just 3)
             (m/when true (maybe/just 3)))))

  (t/testing "It returns nil in the monadic context when the condition is false"
    (t/is (= [nil]
             (m/when false [])))))

(t/deftest curry-tests
  #?(:clj
     (t/testing "It can curry single and fixed arity functions automatically"
       (let [cadd2 (m/curry add2)]
         (t/is (= ((cadd2 1) 2)
                  3))
         (t/is (= (cadd2)
                  cadd2))
         (t/is (= (cadd2 1 2)
                  3)))))

  (t/testing "It can curry anonymous functions when providing an arity"
    (let [csum (m/curry 3 (fn [x y z] (+ x y z)))]
      (t/is (= (((csum 1) 2) 3)
               6))
      (t/is (= ((csum 1 2) 3)
               6))
      (t/is (= (((csum) 1 2) 3)
               6))
      (t/is (= (csum 1 2 3)
               6))))

  (t/testing "It can curry variadic functions when providing an arity"
    (let [csum (m/curry 3 +)]
      (t/is (= (((csum 1) 2) 3)
               6))
      (t/is (= ((csum 1 2) 3)
               6))
      (t/is (= (((csum) 1 2) 3)
               6))
      (t/is (= (csum 1 2 3)
               6)))))

(t/deftest foldm-tests
  (letfn [(m-div [x y]
            (if (zero? y)
              (maybe/nothing)
              (maybe/just (/ x y))))]
    (t/testing "It can fold a non-empty collection without an explicit context"
      (t/is (= (maybe/just #?(:clj 1/6 :cljs (/ 1 6)))
               (m/foldm m-div 1 [1 2 3])))
      (t/is (= (maybe/nothing)
               (m/foldm m-div 1 [1 0 3]))))
    (t/testing "It cannot fold an empty collection without an explicit context"
      (t/is (thrown? #?(:clj IllegalArgumentException, :cljs js/Error)
                     (with-redefs [cats.context/get-current (constantly nil)]
                       (m/foldm m-div 1 [])))))
    (t/testing "It can fold a non-empty collection, given an explicit context"
      (t/is (= (maybe/just #?(:clj 1/6, :cljs (/ 1 6)))
               (m/foldm maybe/maybe-monad m-div 1 [1 2 3])))
      (t/is (= (maybe/nothing)
               (m/foldm maybe/maybe-monad m-div 1 [1 0 3]))))
    (t/testing "It can fold an empty collection, given an explicit context"
      (t/is (= (maybe/just 1)
               (m/foldm maybe/maybe-monad m-div 1 [])))
      (t/is (= (maybe/just 1)
               (m/with-monad maybe/maybe-monad
                 (m/foldm m-div 1 [])))))))
