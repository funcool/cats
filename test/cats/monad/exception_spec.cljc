(ns cats.monad.exception-spec
  #?(:cljs
     (:require [cljs.test :as t]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.monad.exception :as exc :include-macros true]
               [cats.monad.either :as either]
               [cats.core :as m :include-macros true])
     :clj
     (:require [clojure.test :as t]
               [cats.builtin :as b]
               [cats.protocols :as p]
               [cats.monad.exception :as exc]
               [cats.monad.either :as either]
               [cats.core :as m])))

(t/deftest basic-operations-test
  (let [e #?(:clj (Exception. "test")
             :cljs (js/Error. "test"))]
    (t/is (= 1 (m/extract (exc/try-on 1))))
    (t/is (= e (m/extract (exc/try-on (throw e)))))
    (t/is (= e (m/extract (exc/try-on e))))))

#?(:clj
   (t/deftest ideref-test
     (t/is (= 1 @(exc/success 1)))
     (t/is (thrown? Exception @(exc/failure {:message "foobar"})))))

(t/deftest predicates-test
  (let [m1 (exc/success 1)
        m2 (exc/failure {})]
    (t/is (exc/success? m1))
    (t/is (exc/failure? m2))))


(t/deftest wrapping-test
  (let [func #?(:clj (fn [x] (+ x nil))
                :cljs (fn [x] (throw (js/Error. "test"))))
        func (exc/wrap func)]
    (t/is (exc/failure? (func 3)))))

#?(:clj
   (t/deftest try-or-else-test
     (let [m1 (exc/try-or-else (+ 1 nil) 40)]
       (t/is (exc/success? m1))
       (t/is (= 40 (exc/extract m1))))))

#?(:clj
   (t/deftest try-or-recover-test
     (let [m1 (exc/try-or-recover (+ 1 nil) (fn [e] (m/return 60)))]
       (t/is (exc/success? m1))
       (t/is (= 60 (exc/extract m1))))

     (let [m1 (exc/try-or-recover
               (+ 1 nil)
               (fn [e] (either/right 60)))]
       (t/is (either/right? m1))
       (t/is (= 60 (either/from-either m1))))))

#?(:cljs
   (t/deftest try-or-recover-test
     (let [e  (js/Error. "test")
           m1 (exc/try-or-recover e (fn [e] (m/return 60)))]
       (t/is (exc/success? m1))
       (t/is (= 60 (exc/extract m1))))

     (let [e  (js/Error. "test")
           m1 (exc/try-or-recover e (fn [e] (either/right 60)))]
       (t/is (either/right? m1))
       (t/is (= 60 (m/extract m1))))))

#?(:clj
   (t/deftest try-on-macro-test
     (let [m1 (exc/try-on (+ 1 nil))]
       (t/is (instance? NullPointerException (m/extract m1))))))

#?(:cljs
   (t/deftest try-on-macro-test
     (let [m1 (exc/try-on (js/Error. "foo"))]
       (t/is (instance? js/Error (m/extract m1))))))

#?(:clj
   (t/deftest functor-test
     (let [m1 (exc/try-on 1)
           m2 (exc/try-on nil)]
       (t/is (instance? NullPointerException
                        (m/extract (m/fmap inc m2))))
       (t/is (= (exc/success 2) (m/fmap inc m1))))))

(t/deftest first-monad-law-left-identity
  (t/is (= (exc/success 2)
           (m/>>= (p/mreturn exc/exception-monad 2)
                  exc/success)))

  (t/is (= (exc/success 2)
           (m/>>= (p/mreturn exc/exception-monad 2)
                  exc/success
                  exc/success))))

(t/deftest second-monad-law-right-identity
  (t/is (= (exc/success 2)
           (m/>>= (exc/try-on 2) m/return))))

(t/deftest third-monad-law-associativity
  (t/is (= (m/>>= (m/mlet [x (exc/try-on 2)
                           y (exc/try-on (inc x))]
                    (m/return y))
                  (fn [y] (exc/try-on (inc y))))
           (m/>>= (exc/try-on 2)
                  (fn [x]
                    (m/>>= (exc/try-on (inc x))
                           (fn [y] (exc/try-on (inc y)))))))))
