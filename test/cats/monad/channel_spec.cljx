(ns cats.monad.channel-spec
  #+cljs
  (:require-macros [cljs.core.async.macros :refer [go]])

  #+cljs
  (:require [cljs.test :as t]
            [cljs.core.async :refer [chan put! take! <! >!]]
            [cats.builtin :as b]
            [cats.protocols :as pt]
            [cats.monad.channel :as channel]
            [cats.monad.either :as either]
            [cats.core :as m :include-macros true])

  #+clj
  (:require [clojure.test :as t]
            [clojure.core.async :refer [go chan put! take! <! >! <!! >!!]]
            [cats.builtin :as b]
            [cats.protocols :as pt]
            [cats.monad.channel :as channel]
            [cats.monad.either :as either]
            [cats.core :as m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Channel Monad Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clj
(t/deftest channel-as-functor
  (let [ch (m/pure channel/channel-monad 1)]
    (t/is (= 2 (<!! (m/fmap inc ch))))))

#+clj
(t/deftest channel-as-monad-1
  (let [ch (m/pure channel/channel-monad 1)]
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
      (let [ch (m/pure channel/channel-monad 1)
            rs (m/fmap inc ch)]
        (t/is (= 2 (<! rs)))
        (done)))))

#+cljs
(t/deftest channel-as-monad-1
  (t/async done
    (go
      (let [ch (m/pure channel/channel-monad 3)
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
      (let [ch1 (m/pure channel/channel-monad 4)
            ch2 (m/pure channel/channel-monad 4)
            vl  (m/>>= ch2 channel/with-value)]
        (t/is (= (<! ch1)
                 (<! vl)))
        (done)))))

#+cljs
(t/deftest second-monad-law-right-identity
  (t/async done
    (go
      (let [ch1 (channel/with-value 2)
            rs  (m/>>= (channel/with-value 2) m/return)]
        (t/is (= (<! ch1) (<! rs)))
        (done)))))

#+cljs
(t/deftest third-monad-law-associativity
  (t/async done
    (go
      (let [rs1 (m/>>= (m/mlet [x  (channel/with-value 2)
                                y  (channel/with-value (inc x))]
                         (m/return y))
                       (fn [y] (channel/with-value (inc y))))
            rs2 (m/>>= (channel/with-value 2)
                       (fn [x] (m/>>= (channel/with-value (inc x))
                                      (fn [y] (channel/with-value (inc y))))))]
        (t/is (= (<! rs1) (<! rs2)))
        (done)))))

(def chaneither-m (either/either-transformer channel/channel-monad))

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

#+cljs
(t/deftest channel-transformer-tests
  (t/async done
    (let [funcright #(channel/with-value (either/right %))
          funcleft #(channel/with-value (either/left %))
          r1 (m/with-monad chaneither-m
               (m/mlet [x (funcright 1)
                        y (funcright 2)]
                 (m/return (+ x y))))

          r2 (m/with-monad chaneither-m
               (m/mlet [x (funcright 1)
                        y (funcleft :foo)
                        z (funcright 2)]
                 (m/return (+ x y))))]
      (go
        (t/is (= (either/right 3) (<! r1)))
        (t/is (= (either/left :foo) (<! r2)))
        (done))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Promise Channel Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#+clj
(defn do-stuff
  "Simple function that returns a promise
  that is resolved in an asyncronous way."
  [v]
  (let [pr (channel/promise)]
    (future
      (put! pr v))
    pr))

#+cljs
(defn do-stuff
  "Simple function that returns a promise
  that is resolved in an asyncronous way."
  [v]
  (let [pr (channel/promise)]
    (js/setTimeout (fn []
                     (put! pr v)) 0)
    pr))


#+clj
(t/deftest take-from-promise-twice
  (let [pr (do-stuff 2)
        r1 (<!! pr)
        r2 (<!! pr)]
    (t/is (= r1 2))
    (t/is (= r2 2)))
  )

#+cljs
(t/deftest take-from-promise-twice
  (t/async done
    (go
      (let [pr (do-stuff 2)
            r1 (<! pr)
            r2 (<! pr)]
        (t/is (= r1 2))
        (t/is (= r2 2))
        (done))))
  )

#+clj
(t/deftest resolve-promise-twice
  (let [pr (do-stuff 2)
        r  (<!! pr)]
    (t/is (= 2 r))

    (>!! pr 3)
    (t/is (= 2 (<!! pr))))
  )

#+cljs
(t/deftest resolve-promise-twice
  (t/async done
    (go
      (let [pr (do-stuff 2)
            r  (<! pr)]
        (t/is (= 2 r))

        (>! pr 3)
        (t/is (= 2 (<! pr)))

        (done))))
  )


#+clj
(t/deftest promise-chains-with-then
  (let [pr1 (do-stuff 2)
        pr2 (channel/then pr1 inc)]
    (t/is (= 3 (<!! pr2)))))

#+cljs
(t/deftest promise-chains-with-then
  (t/async done
    (go
      (let [pr1 (do-stuff 2)
            pr2 (channel/then pr1 inc)
            r1 (<! pr2)]
        (t/is (= 3 r1))
        (done)))))


;; #+cljs
;; (t/deftest promise-chains-with-when
;;   (let [p1 (do-stuff 2)
;;         p2 (do-stuff 3)
;;         p3 (channel/when p1 p2)]
;;     (.log js/console "foobar" p3)
;;     (t/async done
;;       (go
;;         (let [result (<! p3)]
;;           (.log js/console "foobar" result)
;;           (done))))))

;; #+clj
;; (t/deftest experiments
;;   (let [p1 (do-stuff 2)
;;         p2 (do-stuff 3)
;;         p3 (channel/when1 p1 p2)]
;;     (let [result (<!! p3)]
;;       (println "foobar" result))))

