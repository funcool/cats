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

#+clj
(t/deftest channel-monad-tests
  (t/testing "channel as functor"
    (let [ch (m/pure c/channel-monad 1)]
      (t/is (= 2 (<!! (m/fmap inc ch))))))

  (t/testing "channel as monad 1"
    (let [ch (m/pure c/channel-monad 1)]
      (t/is (= 2 (<!! (m/>>= ch (fn [x] (m/return (inc x)))))))))

  (t/testing "channel as monad 2"
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
  )


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


