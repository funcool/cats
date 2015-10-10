(ns cats.labs.crdt.gcounter-spec
  (:require #?(:cljs [cljs.test :as t] :clj [clojure.test :as t])
            [cats.core :as m]
            [cats.labs.crdt :as crdt]
            [cats.labs.crdt.gcounter :as crdt.gcnt]))

(t/deftest constructor-and-initialization-tests
  (let [c1 (crdt.gcnt/gcounter 1)]
    (t/is (= 1 (.-node c1)))
    (t/is (= 0 @c1))))

(t/deftest basic-increment-test
  (let [c1 (crdt/inc (crdt.gcnt/gcounter 1))]
    (t/is (= 1 @c1))))

;; (t/deftest dump-loads-test
;;   (let [c1 (crdt/inc (crdt.gcnt/gcounter 1))
;;         l1 (crdt/dump c1)]
;;     (t/is (= l1 {:type :gcounter :e {1 1}}))
;;     (let [c2 (crdt.gcnt/gcounter l1 2)]
;;       (t/is (= c2 c1)))))

(t/deftest basic-merge-operation-test
  (let [c11 (-> (crdt.gcnt/gcounter 1)
                    (crdt/inc-by 1)
                    (crdt/inc-by 1))
        c12 (-> (crdt.gcnt/gcounter 2)
                     (crdt/inc-by 1)
                     (crdt/inc-by 2))
        result (crdt/merge c11 c12)]
    (t/is (= 1 (.-node result)))
    (t/is (= 5 @result))))

(t/deftest commutativity-test
  (let [c1 (crdt/inc-by (crdt.gcnt/gcounter 1) 1)
        c2 (crdt/inc-by (crdt.gcnt/gcounter 2) 2)
        r1 (crdt/merge c1 c2)
        r2 (crdt/merge c2 c1)]
    (t/is (= r1 r2))))

(t/deftest associativity-test
  (let [c1 (crdt/inc-by (crdt.gcnt/gcounter 1) 1)
        c2 (crdt/inc-by (crdt.gcnt/gcounter 2) 4)
        c3 (crdt/inc-by (crdt.gcnt/gcounter 3) 2)
        r1 (crdt/merge (crdt/merge c1 c2) c3)
        r2 (crdt/merge c1 (crdt/merge c2 c3))]
    (t/is (= r1 r2))))

(t/deftest idempotence-test
  (let [c1 (crdt/inc-by (crdt.gcnt/gcounter 1) 1)
        c2 (crdt/inc-by (crdt.gcnt/gcounter 2) 4)
        r1 (crdt/merge c1 c2)
        r2 (crdt/merge (crdt/merge c1 c2) c2)]
    (t/is (= r1 r2))))



