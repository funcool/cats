(ns cats.monad.promise-spec
  #+cljs
  (:require-macros [cljs.core.async.macros :refer [go]])

  #+cljs
  (:require [cljs.test :as t]
            [cljs.core.async :refer [chan put! take! <! >!]]
            [cats.builtin :as b]
            [cats.protocols :as pt]
            [cats.monad.promise :as p]
            [cats.monad.channel :as c]
            [cats.monad.either :as either]
            [cats.core :as m :include-macros true])

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


