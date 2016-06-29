(ns cats.labs.promise-spec
  #?(:cljs (:require [cljs.test :as t]
                     [cats.builtin :as b]
                     [cats.context :as ctx :include-macros true]
                     [cats.core :as m :include-macros true]
                     [cats.labs.promise :as pm]
                     [promesa.core :as p])
     :clj  (:require [clojure.test :as t]
                     [cats.builtin :as b]
                     [cats.context :as ctx]
                     [cats.core :as m]
                     [cats.labs.promise :as pm]
                     [promesa.core :as p])))


(defn future-ok
  [sleep value]
  (p/promise (fn [resolve reject]
               (p/schedule sleep #(resolve value)))))

(defn future-fail
  [sleep value]
  (p/promise (fn [_ reject]
               (p/schedule sleep #(reject value)))))


#?(:cljs
   (t/deftest extract-from-rejected-promise
     (let [p1 (p/rejected 42)]
       (t/is (p/rejected? p1))
       (t/is (= (p/extract p1) 42)))))

(t/deftest chaining-using-bind
  #?(:cljs
     (t/async done
       (let [p1 (future-ok 200 2)
             inc #(p/resolved (inc %))
             p2 (m/>>= p1 inc inc inc)]
         (p/then p2 (fn [v]
                      (t/is (= v 5))
                      (done)))))
     :clj
     (let [p1 (future-ok 200 2)
           inc #(p/resolved (inc %))
           p2 (m/>>= p1 inc inc inc)]
       (t/is (= @p2 5)))))

(t/deftest promise-as-functor
  #?(:cljs
     (t/async done
       (let [rp (m/fmap inc (p/promise 2))]
         (p/then rp (fn [v]
                      (t/is (= v 3))
                      (done)))))

     :clj
     (let [rp (m/fmap inc (p/promise 2))]
       @(p/then rp (fn [v]
                     (t/is (= v 3)))))))

(t/deftest promise-as-bifunctor
  #?(:cljs
     (t/async done
       (let [rp (m/bimap #(ex-info "Oh no" {}) inc (p/promise 2))]
         (p/then rp (fn [v]
                      (t/is (= v 3))
                      (done)))))

     :clj
     (let [rp (m/bimap #(ex-info "Oh no" {}) inc (p/promise 2))]
       @(p/then rp (fn [v]
                     (t/is (= v 3)))))))

(t/deftest promise-as-applicative
  #?(:cljs
     (t/async done
       (let [rp (m/fapply (p/resolved inc) (p/promise 2))]
         (p/then rp (fn [v]
                      (t/is (= v 3))
                      (done)))))
     :clj
     (let [rp (m/fapply (p/resolved inc) (p/promise 2))]
       @(p/then rp (fn [v]
                     (t/is (= v 3)))))))

(t/deftest promise-as-monad
  #?(:cljs
     (t/async done
       (let [p1 (m/>>= (p/promise 2) (fn [v] (m/return (inc v))))]
         (p/then p1 (fn [v]
                      (t/is (= v 3))
                      (done)))))
     :clj
     (let [p1 (m/>>= (p/promise 2) (fn [v] (m/return (inc v))))]
       @(p/then p1 (fn [v]
                     (t/is (= v 3)))))))

(t/deftest first-monad-law-left-identity
  #?(:cljs
     (t/async done
       (let [p1 (m/pure pm/promise-context 4)
             p2 (m/pure pm/promise-context 4)
             vl  (m/>>= p2 #(m/pure pm/promise-context %))]
         (p/then (p/all [p1 vl])
                 (fn [[v1 v2]]
                   (t/is (= v1 v2))
                   (done)))))
     :clj
     (let [p1 (m/pure pm/promise-context 4)
           p2 (m/pure pm/promise-context 4)
           vl  (m/>>= p2 #(m/pure pm/promise-context %))]
       @(p/then (p/all [p1 vl])
                (fn [[v1 v2]]
                  (t/is (= v1 v2)))))))

(t/deftest second-monad-law-right-identity
  #?(:cljs
     (t/async done
       (let [p1 (p/promise 3)
             rs  (m/>>= (p/promise 3) m/return)]
         (p/then (p/all [p1 rs])
                 (fn [[v1 v2]]
                   (t/is (= v1 v2))
                   (done)))))
     :clj
     (let [p1 (p/promise 3)
           rs  (m/>>= (p/promise 3) m/return)]
       @(p/then (p/all [p1 rs])
                (fn [[v1 v2]]
                  (t/is (= v1 v2)))))))

(t/deftest third-monad-law-associativity
  #?(:cljs
     (t/async done
       (let [rs1 (m/>>= (m/mlet [x (p/promise 2)
                                 y (p/promise (inc x))]
                          (m/return y))
                        (fn [y] (p/promise (inc y))))
             rs2 (m/>>= (p/promise 2)
                        (fn [x] (m/>>= (p/promise (inc x))
                                       (fn [y] (p/promise (inc y))))))]
         (p/then (p/all [rs1 rs2])
                 (fn [[v1 v2]]
                   (t/is (= v1 v2))
                   (done)))))
     :clj
     (let [rs1 (m/>>= (m/mlet [x (p/promise 2)
                               y (p/promise (inc x))]
                        (m/return y))
                      (fn [y]
                        (p/promise (inc y))))
           rs2 (m/>>= (p/promise 2)
                      (fn [x] (m/>>= (p/promise (inc x))
                                     (fn [y] (p/promise (inc y))))))
           [v1 v2] @(p/all [rs1 rs2])]
       (t/is (= v1 v2)))))



