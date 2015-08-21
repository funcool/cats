(ns cats.labs.continuation-spec
  #?(:cljs
     (:require [cljs.test :as t]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.labs.continuation :as cont]
               [cats.monad.maybe :as maybe]
               [cats.context :as ctx :include-macros true]
               [cats.core :as m :include-macros true])
     :clj
     (:require [clojure.test :as t]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.labs.continuation :as cont]
               [cats.monad.maybe :as maybe]
               [cats.context :as ctx]
               [cats.core :as m])))

(def cont-42 (cont/continuation (fn [c] (c 42))))
(def inc-cont-fn (fn [x]
                   (cont/continuation (fn [c] (c (inc x))))))

(t/deftest first-monad-law-left-identity
  (t/is (= (cont/run-cont cont-42)
           (cont/run-cont (ctx/with-context cont/context
                            (m/>>= (m/return 42)
                                   (fn [v] (cont/continuation (fn [c] c v)))))))))


(t/deftest second-monad-law-right-identity
  (t/is (= (cont/run-cont cont-42)
           (cont/run-cont
            (m/>>= cont-42 m/return)))))

(t/deftest third-monad-law-associativity
  (t/is (= (cont/run-cont
            (m/>>= (m/mlet [x  cont-42
                            y  (inc-cont-fn x)]
                     (m/return y))
                   inc-cont-fn))
           (cont/run-cont
            (m/>>= cont-42
                   (fn [x] (m/>>= (cont/continuation (fn [c] (c (inc x))))
                                  inc-cont-fn)))))))

(t/deftest call-cc-allows-create-resumable-computations
  (let [cc (atom nil)]
    (t/is (= 44 (cont/run-cont (m/mlet [x cont-42
                                        y (cont/call-cc (fn [k]
                                                          (reset! cc k)
                                                          (k 2)))]
                                 (m/return (+ x y))))))
    (t/is (= 45 (cont/run-cont (@cc 3))))
    (t/is (= 46 (cont/run-cont (@cc 4))))))
