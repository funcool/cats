(ns cats.applicative.validation-spec
  #+cljs
  (:require [cljs.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.applicative.validation :as validation]
            [cats.monad.either :as either]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [clojure.test :as t]
            [cats.builtin :as b]
            [cats.protocols :as p]
            [cats.applicative.validation :as validation]
            [cats.monad.either :as either]
            [cats.core :as m]))

(t/deftest basic-operations-test
  (t/is (= 42 (m/extract (validation/ok 42))))
  (t/is (= {:foo "bar"} (m/extract (validation/fail {:foo "bar"})))))

(t/deftest ideref-test
  (t/is (= 1 @(validation/fail 1)))
  (t/is (= 1 @(validation/ok 1))))

(t/deftest predicates-test
  (let [m1 (validation/ok 1)
        m2 (validation/fail 42)]
    (t/is (validation/validation? m1))
    (t/is (validation/validation? m2))
    (t/is (validation/ok? m1))
    (t/is (validation/fail? m2))))

(t/deftest semigroup-test
  (let [fail1 (validation/fail [42])
        fail2 (validation/fail [99])
        ok1 (validation/ok 42)
        ok2 (validation/ok 99)]
    (t/is (= (validation/fail [42 99])
             (m/mappend fail1 fail2)))
    (t/is (= ok1
             (m/mappend ok1 fail1)))
    (t/is (= ok1
             (m/mappend fail1 ok1)))
    (t/is (= ok1
             (m/mappend ok1 ok2)))))

(t/deftest functor-test
  (let [m1 (validation/ok 1)
        m2 (validation/fail 42)]
    (t/is (= (m/fmap inc m1) (validation/ok 2)))
    (t/is (= (m/fmap inc m2) (validation/fail 42)))))

(t/deftest applicative-test
  (let [ok1 (validation/ok 42)
        fail1 (validation/fail {:foo "bar"})
        fail2 (validation/fail {:baz "fubar"})]
    (t/is (= fail1 (m/fapply ok1 fail1)))
    (t/is (= (validation/fail {:foo "bar" :baz "fubar"})
             (m/<*> ok1 fail1 fail2)
             (m/fapply fail1 fail2)))))

(t/deftest either-conversion-test
  (let [ok1 (validation/ok 42)
        fail1 (validation/fail 42)
        left1 (either/left 42)
        right1 (either/right 42)]
    (t/is (= (either/right 42) (validation/validation->either ok1)))
    (t/is (= (either/left 42) (validation/validation->either fail1)))

    (t/is (= (validation/ok 42) (validation/either->validation right1)))
    (t/is (= (validation/fail 42) (validation/either->validation left1)))))
