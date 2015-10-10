(ns cats.labs.crdt.pncounter-spec
  (:require #?(:cljs [cljs.test :as t] :clj [clojure.test :as t])
            [cats.core :as m]
            [cats.labs.crdt :as crdt]
            [cats.labs.crdt.gcounter :as gcnt]
            [cats.labs.crdt.pncounter :as pncnt]))

(t/deftest constructor-and-initialization-tests
  (let [c1 (pncnt/pncounter 1)]
    (t/is (gcnt/gcounter? (.-p c1)))
    (t/is (gcnt/gcounter? (.-n c1)))
    (t/is (= 1 (.-node (.-p c1))))
    (t/is (= 1 (.-node (.-n c1))))
    (t/is (= 0 @c1))))

(t/deftest basic-increment-test
  (let [c1 (crdt/inc (pncnt/pncounter 1))]
    (t/is (= 1 @c1))))

(t/deftest basic-decrement-test
  (let [c1 (crdt/dec (pncnt/pncounter 1))]
    (t/is (= -1 @c1))))

;; (t/deftest dump-loads-test
;;   (let [c1 (-> (pncnt/pncounter 1)
;;                (crdt/inc)
;;                (crdt/inc)
;;                (crdt/dec))
;;         l1 (crdt/dump c1)]
;;     (t/is (= @c1 1))
;;     (t/is (= l1 {:type :pncounter
;;                  :p {:type :gcounter :e {1 2}}
;;                  :n {:type :gcounter :e {1 1}}}))
;;     (let [c2 (pncnt/pncounter l1 2)]
;;       (t/is (= c2 c1)))))

(def c1 (-> (pncnt/pncounter 1)
            (crdt/inc)
            (crdt/inc)
            (crdt/dec)
            (crdt/dec)
            (crdt/inc)))

(def c2 (-> (pncnt/pncounter 2)
            (crdt/dec)
            (crdt/dec)
            (crdt/dec)
            (crdt/dec)
            (crdt/inc)))

(def c3 (-> (pncnt/pncounter 2)
            (crdt/dec)
            (crdt/inc)
            (crdt/dec)
            (crdt/dec)
            (crdt/inc)))

(t/deftest commutativity-test
  (let [r1 (crdt/merge c1 c2)
        r2 (crdt/merge c2 c1)]
    (t/is (= r1 r2))
    (t/is (= @r1 @r2))))

(t/deftest associativity-test
  (let [r1 (crdt/merge (crdt/merge c1 c2) c3)
        r2 (crdt/merge c1 (crdt/merge c2 c3))]
    (t/is (= r1 r2))
    (t/is (= @r1 @r2))))

(t/deftest idempotence-test
  (let [r1 (crdt/merge (crdt/merge c1 c2) c2)
        r2 (crdt/merge c1 c2)]
    (t/is (= r1 r2))
    (t/is (= @r1 @r2))))



