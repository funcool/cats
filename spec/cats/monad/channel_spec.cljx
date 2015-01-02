(ns cats.monad.channel-spec
  ;; #+cljs
  ;; (:require [speclj.core :as s :include-macros true]
  ;;           [cats.builtin :as b]
  ;;           [cats.protocols :as p]
  ;;           [cats.monad.promise :as prom]
  ;;           [cats.core :as m :include-macros true])

  #+clj
  (:require [clojure.core.async :refer [go chan put! take! <! >! <!! >!!]]
            [speclj.core :as s]
            [cats.builtin :as b]
            [cats.protocols :as pt]
            [cats.monad.channel :as c]
            [cats.monad.either :as either]
            [cats.core :as m]))

(s/describe "channel-monad"
  (s/it "channel as functor"
    (let [ch (m/pure c/channel-monad 1)]
      (s/should= 2 (<!! (m/fmap inc ch)))))

  (s/it "channel as monad 1"
    (let [ch (m/pure c/channel-monad 1)]
      (s/should= 2 (<!! (m/>>= ch (fn [x] (m/return (inc x))))))))

  (s/it "channel as monad 2"
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
      (s/should= 3 (<!! r))))
)


(def chaneither-m (either/either-transformer c/channel-monad))

(s/describe "channel-monad-with-either"
  (s/it "channel combination with either"
    (let [func (fn [x] (go (either/right x)))
          r    (m/with-monad chaneither-m
                 (m/mlet [x (func 1)
                          y (func 2)]
                   (m/return (+ x y))))]

      (s/should= (either/right 3) (<!! r)))))


