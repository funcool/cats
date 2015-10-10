(ns cats.labs.crdt
  (:refer-clojure :exclude [+ inc val merge dec])
  (:require [cats.labs.crdt.protocols :as p]
            [cats.protocols :as mp]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Counters Api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn inc
  "Increment the counter value in 1."
  [owner]
  (p/-add owner 1))

(defn inc-by
  "Increment the counter value in `v`."
  [owner v]
  (p/-add owner v))

(defn dec
  "A shortcut for `(inc-by counter -1)`."
  [owner]
  (p/-add owner -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn merge
  "Converge two data structures. Both provided
  data types should satify the JoinSemiLattice
  protocol."
  [owner other]
  (mp/-join owner other))
