(ns cats.labs.sugar-spec
  #?(:cljs (:require-macros [cats.labs.sugar :refer [ap ap-> ap->> as-ap->
                                                   ->= ->>= as->=]]))
  #?(:cljs
     (:require [cljs.test :as t]
               [cats.core :as m :include-macros true]
               [cats.monad.maybe :as maybe])
     :clj
     (:require [clojure.test :as t]
               [cats.core :as m]
               [cats.monad.maybe :as maybe]
               [cats.labs.sugar :refer [ap ap-> ap->> as-ap->
                                        ->= ->>= as->=]])))

(t/deftest ap-example-test
  (t/is (= (ap + (maybe/just 1) (maybe/just 2) (maybe/just 3))
           (maybe/just 6)))

  (t/is (= (ap str ["hi" "lo"] ["bye" "woah" "hey"])
           ["hibye" "hiwoah" "hihey"
            "lobye" "lowoah" "lohey"]))

  (t/is (= (ap-> (maybe/just 1)
                 inc
                 inc)
           (maybe/just 3)))

  (t/is (= (ap->> (maybe/just 1)
                  inc
                  inc
                  (- (maybe/just 5)))
           (maybe/just 2)))

  (t/is (= (as-ap-> (maybe/just 1) $
                    (inc $)
                    (/ (maybe/just 6) $ (maybe/nothing))
                    (inc $))
           (maybe/nothing))))

(t/deftest threading-macros
  (let [linc (comp m/return inc)
        ldec (comp m/return dec)
        safe-div (fn [a b] (if (> b 0) (maybe/just (/ a b)) (maybe/nothing)))]
    (t/is (= (->= (maybe/just 1)
                  linc
                  linc
                  linc)
             (maybe/just 4)))
    (t/is (= (->>= (maybe/just 1)
                   ldec
                   (safe-div 5)
                   linc)
             (maybe/nothing)))
    (t/is (= (as->= (maybe/just 1) $
                    (m/return (+ 2 $ 3))
                    (safe-div 6 $)
                    (linc $))
             (maybe/just 2)))))
