(ns cats.labs.channel-spec
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]]))
  #?(:cljs (:require [cljs.core.async :as a]
                     [cljs.test :as t]
                     [cats.builtin :as b]
                     [cats.context :as ctx :include-macros true]
                     [cats.core :as m :include-macros true]
                     [cats.labs.channel :as c]
                     [cats.monad.either :as either])
     :clj  (:require [clojure.core.async :as a :refer [go]]
                     [clojure.test :as t]
                     [cats.builtin :as b]
                     [cats.context :as ctx]
                     [cats.core :as m]
                     [cats.labs.channel :as c]
                     [cats.monad.either :as either])))

(t/deftest channel-as-functor
  #?(:clj
     (let [ch (m/pure c/context 1)]
       (t/is (= 2 (a/<!! (m/fmap inc ch)))))

     :cljs
     (t/async done
       (go
         (let [ch (m/pure c/context 1)
               rs (m/fmap inc ch)]
           (t/is (= 2 (a/<! rs)))
           (done))))))

(t/deftest channel-as-monad-1
  #?(:clj
     (let [ch (m/pure c/context 1)]
       (t/is (= 2 (a/<!! (m/>>= ch (fn [x] (m/return (inc x))))))))

     :cljs
     (t/async done
       (go
         (let [ch (m/pure c/context 1)
               result (m/>>= ch (fn [x] (m/return (inc x))))]
           ;; (println 2222 @(a/<! result))

           (t/is (= 2 (a/<! result)))
           (done))))))


(t/deftest channel-as-monad-2
  #?(:clj
     (let [ch1 (a/chan 1)
           ch2 (a/chan 1)
           ch3 (a/chan 1)
           r   (m/mlet [x ch1
                        y ch2
                        z ch3]
                 (m/return (+ x y z)))]
       (go
         (a/>! ch1 1)
         (a/>! ch2 1)
         (a/>! ch3 1))
       (t/is (= 3 (a/<!! r))))

     :cljs
     (t/async done
       (go
         (let [ch1 (a/chan 1)
               ch2 (a/chan 1)
               ch3 (a/chan 1)
               r   (m/mlet [x ch1
                            y ch2
                            z ch3]
                     (m/return (+ x y z)))]
           (a/>! ch1 1)
           (a/>! ch2 1)
           (a/>! ch3 1)
           (t/is (= 3 (a/<! r))))
         (done)))))

(t/deftest channel-with-empty
  #?(:clj
     (let [r (m/mlet [v (a/to-chan [1])
                      empty (a/to-chan [])]
                     (m/return v))]
       (t/is (= nil (a/<!! r))))
     :cljs
     (t/async done
        (go
          (let [r (m/mlet [v (a/to-chan [1])
                           empty (a/to-chan [])]
                          (m/return v))]
            (t/is (= nil (a/<! r)))
            (done))))))

(t/deftest channel-comprehension
  #?(:clj
     (let [r (m/mlet [x (a/to-chan (range 1 4))
                      y (a/to-chan (range x))]
                     (m/return [x y]))]
       (t/is (= [[1 0] [2 0] [2 1] [3 0] [3 1] [3 2]]
                (a/<!! (a/into [] r)))))
     :cljs
     (t/async done
              (go
                (let [r (m/mlet [x (a/to-chan (range 1 4))
                                 y (a/to-chan (range x))]
                                (m/return [x y]))]
                  (t/is (= [[1 0] [2 0] [2 1] [3 0] [3 1] [3 2]]
                           (a/<! (a/into [] r))))
                  (done))))))


(t/deftest first-monad-law-left-identity
  #?(:clj
     (let [ch1 (m/pure c/context 4)
           ch2 (m/pure c/context 4)
           vl  (m/>>= ch2 c/with-value)]
       (t/is (= (a/<!! ch1)
                (a/<!! vl))))

     :cljs
     (t/async done
       (go
         (let [ch1 (m/pure c/context 4)
               ch2 (m/pure c/context 4)
               vl  (m/>>= ch2 c/with-value)]
           (t/is (= (a/<! ch1)
                    (a/<! vl)))
           (done))))))

(t/deftest second-monad-law-right-identity
  #?(:clj
     (let [ch1 (c/with-value 2)
           rs  (m/>>= (c/with-value 2) m/return)]
       (t/is (= (a/<!! ch1) (a/<!! rs))))

     :cljs
     (t/async done
       (go
         (let [ch1 (c/with-value 2)
               rs  (m/>>= (c/with-value 2) m/return)]
           (t/is (= (a/<! ch1) (a/<! rs)))
           (done))))))

(t/deftest third-monad-law-associativity
  #?(:clj
     (let [rs1 (m/>>= (m/mlet [x  (c/with-value 2)
                               y  (c/with-value (inc x))]
                        (m/return y))
                      (fn [y] (c/with-value (inc y))))
           rs2 (m/>>= (c/with-value 2)
                      (fn [x] (m/>>= (c/with-value (inc x))
                                     (fn [y] (c/with-value (inc y))))))]
       (t/is (= (a/<!! rs1) (a/<!! rs2))))

     :cljs
     (t/async done
       (go
         (let [rs1 (m/>>= (m/mlet [x  (c/with-value 2)
                                   y  (c/with-value (inc x))]
                            (m/return y))
                          (fn [y] (c/with-value (inc y))))
               rs2 (m/>>= (c/with-value 2)
                          (fn [x] (m/>>= (c/with-value (inc x))
                                         (fn [y] (c/with-value (inc y))))))]
           (t/is (= (a/<! rs1) (a/<! rs2)))
           (done))))))

(t/deftest semigroup-tests
  #?(:clj
     (let [c1 (a/to-chan [1 2 3])
           c2 (a/to-chan [4 5 6])
           r (m/mappend c1 c2)]
       (t/is (= #{1 2 3 4 5 6} (a/<!! (a/into #{} r)))))
     :cljs
     (t/async done
       (go
         (let [c1 (a/to-chan [1 2 3])
               c2 (a/to-chan [4 5 6])
               r (m/mappend c1 c2)]
           (t/is (= #{1 2 3 4 5 6} (a/<! (a/into #{} r))))
           (done))))))


(t/deftest semigroup-with-monoid-tests
  #?(:clj
     (let [c (m/mappend (m/mempty c/context) (a/to-chan [1]))]
       (t/is (= [1] (a/<!! (a/into [] c)))))
     :cljs
     (t/async done
       (go
         (let [c (m/mappend (m/mempty c/context) (a/to-chan [1]))]
           (t/is (= [1] (a/<! (a/into [] c))))
           (done))))))

(t/deftest applicative-do
  (letfn [(async-call [wait]
            (go
              (a/<! (a/timeout wait))
              wait))]
    #?(:clj
       (let [result (m/alet [x (async-call 100)
                             y (async-call 100)]
                            (+ x y))]
         (t/is (c/channel? result))
         (t/is (= (a/<!! result) 200)))
       :cljs
       (t/async done
         (go
           (let [result (m/alet [x (async-call 100)
                                 y (async-call 100)]
                                (+ x y))]
             (t/is (c/channel? result))
             (t/is (= (a/<! result) 200))
             (done)))))))

(t/deftest monadzero-tests
  #?(:clj
     (t/is (= #{} (a/<!! (a/into #{} (m/mzero c/context)))))
     :cljs
     (t/async
      done
      (go (t/is (= #{} (a/<! (a/into #{} (m/mzero c/context)))))
          (done)))))

(t/deftest monadplus-tests
  #?(:clj
     (do (t/is (= #{1 2 3}
                  (a/<!! (a/into #{} (m/mplus (m/mzero c/context)
                                              (a/to-chan #{1 2 3}))))))
         (t/is (= #{1 2 3}
                  (a/<!! (a/into #{} (m/mplus (a/to-chan #{1 2 3})
                                              (m/mzero c/context))))))
         (t/is (= (a/<!! (a/into #{} (m/mplus (a/to-chan [1])
                                              (m/mplus (a/to-chan [2])
                                                       (a/to-chan [3])))))
                  (a/<!! (a/into #{} (m/mplus (m/mplus (a/to-chan [1])
                                                       (a/to-chan [2]))
                                              (a/to-chan [3])))))))
     :cljs
     (t/async
      done
      (go (t/is (= #{1 2 3}
                   (a/<! (a/into #{} (m/mplus (m/mzero c/context)
                                              (a/to-chan #{1 2 3}))))))
          (t/is (= #{1 2 3}
                   (a/<! (a/into #{} (m/mplus (a/to-chan #{1 2 3})
                                              (m/mzero c/context))))))
          (t/is (= (a/<! (a/into #{} (m/mplus (a/to-chan [1])
                                              (m/mplus (a/to-chan [2])
                                                       (a/to-chan [3])))))
                   (a/<! (a/into #{} (m/mplus (m/mplus (a/to-chan [1])
                                                       (a/to-chan [2]))
                                              (a/to-chan [3]))))))
          (done)))))

(t/deftest channel-mlet-when
  #?(:clj
     (t/is (= [2] (a/<!! (a/into [] (m/mlet [v (a/to-chan [1 2])
                                             :when (even? v)]
                                            (m/return v))))))
     :cljs
     (t/async done
              (go (t/is (= [2] (a/<! (a/into [] (m/mlet [v (a/to-chan [1 2])
                                                         :when (even? v)]
                                                        (m/return v))))))
                  (done)))))



;; #?(:cljs (defn main [] (node/run-tests)))

;; #? (:cljs (set! *main-cli-fn* main))
