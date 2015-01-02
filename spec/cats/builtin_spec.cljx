(ns cats.builtin-spec
  #+cljs
  (:require [speclj.core :as s :include-macros true]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [speclj.core :as s]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.monad.maybe :as maybe]
            [cats.core :as m]))

(s/describe "test-nil-as-maybe"
  (s/it "Nil works like nothing (for avoid unnecesary null pointers)."
    (s/should= (m/>>= nil (fn [_] (m/return 1))) nil)
    (s/should= (m/fmap inc nil) nil)
    (s/should (maybe/nothing? nil))
    (s/should (maybe/maybe? nil)))

  (s/it "get-value function"
    (s/should= (p/get-value nil) nil)))

(s/describe "vector-monad"
  (s/it "The first monad law: left identity"
    (s/should= [1 2 3 4 5]
               (m/>>= [0 1 2 3 4]
                      (fn [x] [(inc x)]))))

  (s/it "The second law: right identity"
    (s/should= [1 2 3]
               (m/>>= [1 2 3]
                      m/return)))

  (s/it "The third law: associativity"
    (s/should= (m/>>= (m/mlet [x [1 2 3 4 5]
                               y [(inc x)]]
                        (m/return y))
                      (fn [z] [(inc z)]))
               (m/>>= [1 2 3 4 5]
                      (fn [x] (m/>>= [(inc x)]
                                     (fn [y] [(inc y)])))))))

(s/describe "sequence-monad"
  (let [val->lazyseq (fn [x] (lazy-seq [x]))
        s (val->lazyseq 2)]

    (s/it "The first monad law: left identity"
      (s/should= s (m/with-monad b/sequence-monad
                     (m/>>= (m/return 2)
                            val->lazyseq))))

    (s/it "The second monad law: right identity"
      (s/should= s (m/>>= s m/return)))

    (s/it "The third monad law: associativity"
      (s/should= (m/>>= (m/mlet [x s
                                 y  (val->lazyseq (inc x))]
                          (m/return y))
                        (fn [y] (val->lazyseq (inc y))))
                 (m/>>= s
                        (fn [x]
                          (m/>>= (val->lazyseq (inc x))
                                 (fn [y] (val->lazyseq (inc y))))))))))


(s/describe "set-monad"
  (s/it "The first monad law: left identity"
    (s/should= #{2} (m/with-monad b/set-monad
                      (m/>>= (m/return 2)
                             (fn [x] #{x})))))

  (s/it "The second monad law: right identity"
    (s/should= #{2} (m/>>= #{2} m/return)))

  (s/it "The third monad law: associativity"
    (s/should= (m/>>= (m/mlet [x #{2}
                               y #{(inc x)}]
                        (m/return y))
                      (fn [y] #{(inc y)}))
               (m/>>= #{2}
                      (fn [x]
                        (m/>>= #{(inc x)}
                               (fn [y] #{(inc y)})))))))

