(ns cats.labs.sugar-spec
  (:require [clojure.test :as t]
            [cats.core :as m]
            [cats.monad.maybe :refer [just nothing]]
            [cats.labs.sugar :refer [ap ap-> ap->> as-ap->
                                     ->= ->>= as->=]]
            ))

(t/deftest ap-example
  (t/is (= (ap + (just 1) (just 2) (just 3))
           (just 6)))
  (t/is (= (ap str ["hi" "lo"] ["bye" "woah" "hey"])
           ["hibye" "hiwoah" "hihey"
            "lobye" "lowoah" "lohey"]))
  (t/is (= (ap-> (just 1)
                 inc
                 inc)
           (just 3)))
  (t/is (= (ap->> (just 1)
                  inc
                  inc
                  (- (just 5)))
           (just 2)))
  (t/is (= (as-ap-> (just 1) $
                    (inc $)
                    (/ (just 6) $ (nothing))
                    (inc $))
           (nothing))))

(t/deftest threading-macros
  (let [linc (comp m/return inc)
        ldec (comp m/return dec)
        safe-div (fn [a b] (if (> b 0) (just (/ a b)) (nothing)))]
    (t/is (= (->= (just 1)
                  linc
                  linc
                  linc)
             (just 4)))
    (t/is (= (->>= (just 1)
                   ldec
                   (safe-div 5)
                   linc)
             (nothing)))
    (t/is (= (as->= (just 1) $
                    (m/return (+ 2 $ 3))
                    (safe-div 6 $)
                    (linc $))
             (just 2)))))
