(ns cats.labs.sugar-spec
  (:require [clojure.test :as t]
            [cats.core :as m]
            [cats.monad.maybe :refer [just nothing]]
            [cats.labs.sugar :refer [ap]]
            ))

(t/deftest ap-example
  (t/is (= (ap + (just 1) (just 2) (just 3))
           (just 6)))
  (t/is (= (ap str ["hi" "lo"] ["bye" "woah" "hey"])
           ["hibye" "hiwoah" "hihey"
            "lobye" "lowoah" "lohey"])))
