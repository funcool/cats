(ns cats.labs.crdt.gcounter
  "A G-Counter is a grow-only counter (inspired by vector clocks) in which
  only increment and merge are possible. Incrementing the counter adds 1
  to the count for the current actor.

  Divergent histories are resolved by taking the maximum count for each actor
  (like a vector clock merge). The value of the counter is the sum of all
  actor counts."
  (:require [cats.labs.crdt.protocols :as p]
            [cats.protocols :as mp]))

(deftype GCounter [e node]
  #?@(:clj [Object
            (equals [self other]
              (if (instance? GCounter other)
                (= e (.-e other))
                false))
            clojure.lang.IDeref
            (deref [it] (reduce + 0 (vals e)))]
      :cljs [cljs.core/IEquiv
             (-equiv [self other]
               (if (instance? GCounter other)
                 (= e (.-e other))
                 false))
             cljs.core/IDeref
             (-deref [it] (reduce + 0 (vals e)))])

  p/ICounter
  (-add [_ delta]
    (assert (number? delta) "delta should be a number")
    (assert (pos? delta) "Only positive delta are allowed.")
    (GCounter. (update e node (fnil #(+ % delta) 0)) node))


  mp/JoinSemiLattice
  (-join [_ other]
    (let [keys (into (set (keys e)) (set (keys (.-e other))))
          res  (reduce (fn [data key]
                         (assoc data key (max (get e key 0)
                                              (get (.-e other) key 0))))
                       {}
                       keys)]
      (GCounter. res node))))

(defn gcounter?
  "Return true if `v` is a instance
  of GCounter type."
  [v]
  (instance? GCounter v))

(defn gcounter*
  [data node]
  {:pre [(map? data) (:e data)]}
  (GCounter. (:e data) node))

(defn gcounter
  "A G-Counter data type constructor."
  ([]
   (GCounter. {} p/*node*))
  ([node]
   (GCounter. {} node)))
