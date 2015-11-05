(ns cats.labs.crdt.pncounter
  "PN-Counters allow the counter to be incremented by tracking the
  increments (P) separate from the decrements (N). Both P and N are represented
  as internal G-Counters.

  Merge is handled by merging the internal P and N counters. The value of the
  counter is the value of the P counter minus the value of the N counter."
  (:require [cats.labs.crdt.protocols :as p]
            [cats.labs.crdt.gcounter :as gcnt]
            [cats.protocols :as mp]
            [cats.util :as util]))

(defn- make-positive
  [v]
  #?(:clj (Math/abs v)
     :cljs (js/Math.abs v)))

(deftype PNCounter [p n]
  #?@(:clj [Object
            (equals [self other]
              (if (instance? PNCounter other)
                (and (= p (.-p other))
                     (= n (.-n other)))
                false))
            clojure.lang.IDeref
            (deref [it] (- @p @n))]
      :cljs [cljs.core/IEquiv
             (-equiv [self other]
               (if (instance? PNCounter other)
                 (and (= p (.-p other))
                      (= n (.-n other)))
                 false))
             cljs.core/IDeref
             (-deref [it] (- @p @n))])

  mp/Printable
  (-repr [it]
    (str "#<PNCounter value=" @it ">"))

  p/ICounter
  (-add [this delta]
    (assert (number? delta) "delta should be a number")
    (cond
      (pos? delta) (PNCounter. (p/-add p delta) n)
      (neg? delta) (PNCounter. p (p/-add n (make-positive delta)))
      :else this))

  mp/JoinSemiLattice
  (-join [_ other]
    (PNCounter. (mp/-join p (.-p other))
                (mp/-join n (.-n other)))))

(util/make-printable PNCounter)

(defn pncounter*
  [data node]
  {:pre [(map? data)
         (map? (:p data))
         (map? (:n data))]}
  (PNCounter.
   (gcnt/gcounter* (:p data) node)
   (gcnt/gcounter* (:n data) node)))

(defn pncounter
  "A G-Counter data type constructor."
  ([]
   (PNCounter. (gcnt/gcounter)
               (gcnt/gcounter)))
  ([node]
   (PNCounter. (gcnt/gcounter node)
               (gcnt/gcounter node))))


