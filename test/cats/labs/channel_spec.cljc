(ns cats.labs.channel-spec
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]]))
  (:require #?@(:clj  [[clojure.core.async :as a :refer [go]]
                       [clojure.test :as t]]
                :cljs [
                       [cljs.core.async :as a]
                       [cljs.test :as t]])
            [cats.core :as m]
            [cats.labs.channel :as c]))

(t/deftest channel-as-functor
  #?(:clj
     (let [ch (m/pure c/channel-monad 1)]
       (t/is (= 2 (a/<!! (m/fmap inc ch)))))

     :cljs
     (t/async done
       (go
         (let [ch (m/pure c/channel-monad 1)
               rs (m/fmap inc ch)]
           (t/is (= 2 (a/<! rs)))
           (done))))))

(t/deftest channel-as-monad-1
  #?(:clj
     (let [ch (m/pure c/channel-monad 1)]
       (t/is (= 2 (a/<!! (m/>>= ch (fn [x] (m/return (inc x))))))))

     :cljs
     (t/async done
       (go
         (let [ch (m/pure c/channel-monad 3)
               rs (m/>>= ch (fn [x] (m/return (inc x))))]
           (t/is (= 4 (a/<! rs)))
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

(t/deftest first-monad-law-left-identity
  #?(:clj
     (let [ch1 (m/pure c/channel-monad 4)
           ch2 (m/pure c/channel-monad 4)
           vl  (m/>>= ch2 c/with-value)]
       (t/is (= (a/<!! ch1)
                (a/<!! vl))))

     :cljs
     (t/async done
       (go
         (let [ch1 (m/pure c/channel-monad 4)
               ch2 (m/pure c/channel-monad 4)
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

(defn async-call
  [wait]
  (a/go
    (a/<! (a/timeout wait))
    wait))

#?(:clj
   (t/deftest applicative-do
     (let [result (m/alet [x (async-call 100)
                           y (async-call 100)]
                    (+ x y))]
       (t/is (c/channel? result))
       (t/is (a/<!! result) 200))))


;; (def chaneither-m (either/either-transformer c/channel-monad))

;; #?(:clj
;;    (t/deftest channel-transformer-tests
;;      (t/testing "channel combination with either"
;;        (let [funcright (fn [x] (go (either/right x)))
;;              funcleft (fn [x] (go (either/left x)))
;;              r1 (m/with-monad chaneither-m
;;                   (m/mlet [x (funcright 1)
;;                            y (funcright 2)]
;;                     (m/return (+ x y))))

;;              r2 (m/with-monad chaneither-m
;;                   (m/mlet [x (funcright 1)
;;                            y (funcleft :foo)
;;                            z (funcright 2)]
;;                     (m/return (+ x y))))]

;;          (t/is (= (either/right 3) (a/<!! r1)))
;;          (t/is (= (either/left :foo) (a/<!! r2))))))

;;    :cljs
;;    (t/deftest channel-transformer-tests
;;      (t/async done
;;               (let [funcright #(c/with-value (either/right %))
;;                     funcleft #(c/with-value (either/left %))
;;                     r1 (m/with-monad chaneither-m
;;                          (m/mlet [x (funcright 1)
;;                                   y (funcright 2)]
;;                            (m/return (+ x y))))

;;                     r2 (m/with-monad chaneither-m
;;                          (m/mlet [x (funcright 1)
;;                                   y (funcleft :foo)
;;                                   z (funcright 2)]
;;                            (m/return (+ x y))))]
;;                 (go
;;                   (t/is (= (either/right 3) (a/<! r1)))
;;                   (t/is (= (either/left :foo) (a/<! r2)))
;;                   (done))))))

;; #?(:cljs (defn main [] (node/run-tests)))

;; #? (:cljs (set! *main-cli-fn* main))
