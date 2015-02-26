(ns cats.monad.promise-spec
  #+cljs
  (:require-macros [cljs.core.async.macros :refer [go]])

  #+cljs
  (:require [cljs.test :as t]
            [cljs.core.async :refer [chan put! take! <! >! timeout]]
            [cats.builtin :as b]
            [cats.protocols :as pt]
            [cats.monad.promise :as p]
            [cats.monad.channel :as c]
            [cats.monad.either :as either]
            [cats.core :as m])

  #+clj
  (:require [clojure.test :as t]
            [clojure.core.async :refer [go chan put! take! <! >! <!! >!!]]
            [cats.builtin :as b]
            [cats.protocols :as pt]
            [cats.monad.promise :as p]
            [cats.monad.channel :as channel]
            [cats.monad.either :as either]
            [cats.core :as m]))

#+clj
(defn do-stuff
  "Simple function that returns a promise
  that is resolved in an asyncronous way."
  [v]
  (let [pr (p/promise)]
    (future
      (put! pr v))
    pr))

#+cljs
(defn do-stuff
  "Simple function that returns a promise
  that is resolved in an asyncronous way."
  [v]
  (let [pr (p/promise)]
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
        pr2 (p/then pr1 inc)]
    (t/is (= 3 (<!! pr2)))))

#+cljs
(t/deftest promise-chains-with-then
  (t/async done
    (go
      (let [pr1 (do-stuff 2)
            pr2 (p/then pr1 inc)
            r1 (<! pr2)]
        (t/is (= 3 r1))
        (done)))))


;; #+cljs
;; (t/deftest promise-chains-with-when
;;   (let [p1 (do-stuff 2)
;;         p2 (do-stuff 3)
;;         p3 (p/when p1 p2)]
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
;;         p3 (p/when1 p1 p2)]
;;     (let [result (<!! p3)]
;;       (println "foobar" result))))

#+cljs
(t/deftest first-monad-law-left-identity
  (t/async done
    (go
      (let [ch1 (m/pure c/channel-monad 4)
            ch2 (m/pure c/channel-monad 4)
            vl  (m/>>= ch2 p/promise)]
        (t/is (= (<! ch1)
                 (<! vl)))
        (done)))))

#+cljs
(t/deftest second-monad-law-right-identity
  (t/async done
    (go
      (let [ch1 (p/promise 2)
            rs  (m/>>= (p/promise 2) m/return)]
        (t/is (= (<! ch1) (<! rs)))
        (done)))))

#+cljs
(t/deftest third-monad-law-associativity
  (t/async done
    (let [rs1 (m/>>= (m/mlet [x (do-stuff 2)
                              y (do-stuff (inc x))]
                       (m/return y))
                     (fn [y] (do-stuff (inc y))))
          rs2 (m/>>= (do-stuff 2)
                     (fn [x] (m/>>= (do-stuff (inc x))
                                    (fn [y] (do-stuff (inc y))))))]
      (go
        (t/is (= (<! rs1) (<! rs2)))
        (done)))))

