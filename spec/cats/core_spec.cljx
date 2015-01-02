(ns cats.core-spec
  #+cljs
  (:require [speclj.core :as s :include-macros true]
            [cats.builtin :as b]
            [cats.monad.maybe :as maybe]
            [cats.core :as m :include-macros true])
  #+clj
  (:require [speclj.core :as s]
            [cats.builtin :as b]
            [cats.monad.maybe :as maybe]
            [cats.core :as m]))


(s/describe "mlet (do-syntax)"
  (s/it "Support regular let bindings inside mlet"
    (s/should= (maybe/just 2)
               (m/mlet [i (maybe/just 1)
                        :let [i (inc i)]]
                 (m/return i))))

  (s/it "Support :when guards inside its bindings"
    (s/should= (maybe/nothing)
               (m/mlet [i (maybe/just 2)
                        :when (> i 2)]
                 (m/return i)))
    (s/should= [3 4 5]
               (m/mlet [i [1 2 3 4 5]
                      :when (> i 2)]
                 (m/return i))))
  (s/it "The body runs in an implicit do"
    (s/should= (maybe/just 3)
               (m/mlet [i (maybe/just 2)
                        :let [x (inc i)]]
                 (assert (= x 3))
                 (m/return x)))))

(s/describe "sequence"
  (s/it "It works with vectors"
    (s/should= (m/sequence [[1 2] [3 4]])
               [[1 3] [1 4] [2 3] [2 4]]))

  (s/it "It works with lazy seqs"
    (s/should= (m/sequence [(lazy-seq [1 2]) (lazy-seq [3 4])])
               [[1 3] [1 4] [2 3] [2 4]]))

  (s/it "It works with sets"
    (s/should= (m/sequence [#{1 2} #{3 4}])
               #{[1 3] [1 4] [2 3] [2 4]}))

  (s/it "It works with Maybe values"
    (s/should= (maybe/just [2 3])
               (m/sequence [(maybe/just 2) (maybe/just 3)]))

    (s/should= (maybe/nothing)
               (m/sequence [(maybe/just 2) (maybe/nothing)]))))


(s/describe "mapseq"
  (s/it "It works with maybe values"
    (s/should= (m/mapseq maybe/just [1 2 3 4 5])
               (maybe/just [1 2 3 4 5]))
    (s/should= (maybe/nothing)
               (m/mapseq (fn [v]
                           (if (odd? v)
                             (maybe/just v)
                             (maybe/nothing)))
                         [1 2 3 4 5]))))

(s/describe "lift-m"
  (let [monad+ (m/lift-m 2 +)]
    (s/it "It can lift a function to the vector monad"
      (s/should= [1 2 3 4 5 6]
                 (monad+ [0 2 4] [1 2])))

    (s/it "It can lift a function to the Maybe monad"
      (s/should= (maybe/just 6)
                 (monad+ (maybe/just 2) (maybe/just 4)))
      (s/should= (maybe/nothing)
                 (monad+ (maybe/just 1) (maybe/nothing))))

    #+clj
    (s/it "It can lift a function to a Monad Transformer"
      (let [maybe-sequence-monad (maybe/maybe-transformer b/sequence-monad)]
        (s/should= [(maybe/just 1) (maybe/just 2)
                    (maybe/just 3) (maybe/just 4)
                    (maybe/just 5) (maybe/just 6)]
                   (m/with-monad maybe-sequence-monad
                     (monad+ [(maybe/just 0) (maybe/just 2) (maybe/just 4)]
                             [(maybe/just 1) (maybe/just 2)])))))))

(s/describe "filter"
  (s/it "It can filter Maybe monadic values"
    (let [bigger-than-4 (partial < 4)]
      (s/should= (maybe/just 6)
                 (m/filter bigger-than-4 (maybe/just 6)))
      (s/should= (maybe/nothing)
                 (m/filter bigger-than-4 (maybe/just 3)))))
  (s/it "It can filter vectors"
    (s/should= [1 3 5]
               (m/filter odd? [1 2 3 4 5 6]))))

(s/describe "when"
  (s/it "It returns the monadic value unchanged when the condition is true"
    (s/should= (maybe/just 3)
               (m/when true (maybe/just 3))))

  (s/it "It returns nil in the monadic context when the condition is false"
    (s/should= [nil]
               (m/when false []))))
