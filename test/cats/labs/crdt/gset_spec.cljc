(ns cats.labs.crdt.gset-spec
  (:require #?(:cljs [cljs.test :as t] :clj [clojure.test :as t])
            [cats.core :as m]
            [cats.labs.crdt :as crdt]
            [cats.labs.crdt.gset :as gset]))

(t/deftest constructor-and-initialization-test
  (let [c1 (gset/gset)]
    (t/is (gset/gset? c1))))

(t/deftest standard-set-behavior-test
  (let [c1 (gset/gset)
        c2 (conj c1 1)
        c3 (conj c2 2)
        c4 (conj c3 2)]
    (t/is (= #{1 2} (into #{} c4)))
    (t/is (= #{1 2} (into #{} c3)))
    (t/is (= #{1} (into #{} c2)))
    (t/is (= 1 (c2 1)))
    (t/is (= nil (c2 10)))
    (t/is (= c3 c4)))
  (let [c1 (gset/gset)
        c2 (conj c1 1)]
    (t/is (thrown? #?(:cljs cljs.core.ExceptionInfo
                      :clj clojure.lang.ExceptionInfo)
                   (disj c2 1)))))

(t/deftest commutativity-test
  (let [c1 (conj (gset/gset) 1)
        c2 (conj (gset/gset) 2)
        r1 (crdt/merge c1 c2)
        r2 (crdt/merge c2 c1)]
    (t/is (= r1 r2))))

(t/deftest associativity-test
  (let [c1 (conj (gset/gset) 1)
        c2 (conj (gset/gset) 2)
        c3 (conj (gset/gset) 3)
        r1 (crdt/merge (crdt/merge c1 c2) c3)
        r2 (crdt/merge c1 (crdt/merge c2 c3))]
    (t/is (= r1 r2))))

(t/deftest idempotence-test
  (let [c1 (conj (gset/gset) 1)
        c2 (conj (gset/gset) 2)
        r1 (crdt/merge c1 c2)
        r2 (crdt/merge (crdt/merge c1 c2) c2)]
    (t/is (= r1 r2))))

(t/deftest serializability-test
  (let [c1 (conj (gset/gset) 1)
        c2 (conj c1 2)
        r1 (crdt/encode c2)]
    (t/is (= r1 "{:type :cats.labs.crdt/gset, :s #{1 2}}"))
    (t/is (= c2 (crdt/decode r1)))))
