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
  (t/testing "It works with maybe values"
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

(defn add2 [x y]
  (+ x y))

(t/deftest curry-tests
  (t/testing "It can curry single and fixed arity functions automatically"
    (let [cadd2 (m/curry add2)]
      (t/is (= ((cadd2 1) 2)
               3))
      (t/is (= (cadd2)
               cadd2))
      (t/is (= (cadd2 1 2)
               3))))

  (t/testing "It can curry anonymous functions when providing an arity"
    (let [csum (m/curry 3 (fn [x y z] (+ x y z)))]
      (t/is (= (((csum 1) 2) 3)
               6))
      (t/is (= ((csum 1 2) 3)
               6))
      (t/is (= (((csum) 1 2) 3)
               6))
      (t/is (= (csum 1 2 3)
               6))
      ))

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
