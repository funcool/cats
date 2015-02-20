(ns cats.monad.channel-spec
  #+cljs
  (:require-macros [cljs.core.async.macros :refer [go]])

  #+cljs
  (:require [cljs.test :as t]
            [cljs.core.async :refer [chan put! take! <! >!]]
            [cats.builtin :as b]
            [cats.protocols :as pt]
            [cats.monad.channel :as c]
            [cats.monad.either :as either]
            [cats.core :as m :include-macros true])

  #+clj
  (:require [clojure.test :as t]
            [clojure.core.async :refer [go chan put! take! <! >! <!! >!!]]
            [cats.builtin :as b]
            [cats.protocols :as pt]
            [cats.monad.channel :as c]
            [cats.monad.either :as either]
            [cats.core :as m]))

(defn chan-with-value
  [value]
  (let [c (chan)]
    (put! c value)
    c))

#+clj
(t/deftest channel-as-functor
  (let [ch (m/pure c/channel-monad 1)]
    (t/is (= 2 (<!! (m/fmap inc ch))))))

#+clj
(t/deftest channel-as-monad-1
  (let [ch (m/pure c/channel-monad 1)]
    (t/is (= 2 (<!! (m/>>= ch (fn [x] (m/return (inc x)))))))))

#+clj
(t/deftest channel-as-monad-2
    (let [ch1 (chan 1)
          ch2 (chan 1)
          ch3 (chan 1)
          r   (m/mlet [x ch1
                       y ch2
                       z ch3]
                (m/return (+ x y z)))]
      (go
        (>! ch1 1)
        (>! ch2 1)
        (>! ch3 1))
      (t/is (= 3 (<!! r)))))

#+cljs
(t/deftest channel-as-functor
  (t/async done
    (go
      (let [ch (m/pure c/channel-monad 1)
            rs (m/fmap inc ch)]
        (t/is (= 2 (<! rs)))
        (done)))))

#+cljs
(t/deftest channel-as-monad-1
  (t/async done
    (go
      (let [ch (m/pure c/channel-monad 3)
            rs (m/>>= ch (fn [x] (m/return (inc x))))]
        (t/is (= 4 (<! rs)))
        (done)))))

#+cljs
(t/deftest channel-as-monad-2
  (t/async done
    (go
      (let [ch1 (chan 1)
            ch2 (chan 1)
            ch3 (chan 1)
            r   (m/mlet [x ch1
                         y ch2
                         z ch3]
                  (m/return (+ x y z)))]
        (>! ch1 1)
        (>! ch2 1)
        (>! ch3 1)
        (t/is (= 3 (<! r))))
      (done))))


#+cljs
(t/deftest first-monad-law-left-identity
  (t/async done
    (go
      (let [ch1 (m/pure c/channel-monad 4)
            ch2 (m/pure c/channel-monad 4)
            vl  (m/>>= ch2 chan-with-value)]
        (t/is (= (<! ch1)
                 (<! vl)))
        (done)))))

#+cljs
(t/deftest second-monad-law-right-identity
  (t/async done
    (go
      (let [ch1 (chan-with-value 2)
            rs  (m/>>= (chan-with-value 2) m/return)]
        (t/is (= (<! ch1) (<! rs)))
        (done)))))

#+cljs
(t/deftest third-monad-law-associativity
  (t/async done
    (go
      (let [rs1 (m/>>= (m/mlet [x  (chan-with-value 2)
                                y  (chan-with-value (inc x))]
                         (m/return y))
                       (fn [y] (chan-with-value (inc y))))
            rs2 (m/>>= (chan-with-value 2)
                       (fn [x] (m/>>= (chan-with-value (inc x))
                                      (fn [y] (chan-with-value (inc y))))))]
        (t/is (= (<! rs1) (<! rs2)))
        (done)))))

#+clj
(def chaneither-m (either/either-transformer c/channel-monad))

#+clj
(t/deftest channel-transformer-tests
  (t/testing "channel combination with either"
    (let [funcright (fn [x] (go (either/right x)))
          funcleft (fn [x] (go (either/left x)))
          r1 (m/with-monad chaneither-m
               (m/mlet [x (funcright 1)
                        y (funcright 2)]
                 (m/return (+ x y))))

          r2 (m/with-monad chaneither-m
               (m/mlet [x (funcright 1)
                        y (funcleft :foo)
                        z (funcright 2)]
                 (m/return (+ x y))))]

      (t/is (= (either/right 3) (<!! r1)))
      (t/is (= (either/left :foo) (<!! r2)))))
  )


