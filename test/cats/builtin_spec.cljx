(ns cats.builtin-spec
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

(t/deftest test-nil-as-maybe
  (t/testing "Nil works like nothing (for avoid unnecesary null pointers)."
    (t/is (= (m/>>= nil (fn [_] (m/return 1))) nil))
    (t/is (= (m/fmap inc nil) nil))
    (t/is (maybe/nothing? nil))
    (t/is (maybe/maybe? nil)))

  (t/testing "extract function"
    (t/is (= (p/extract nil) nil))))

(t/deftest vector-monad
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
