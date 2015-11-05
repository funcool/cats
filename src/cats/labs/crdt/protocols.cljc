(ns cats.labs.crdt.protocols
  (:require [cats.labs.crdt.util :as u]))

(def ^:dynamic *node* (u/hostname))

(defprotocol ICounter
  "A basic protocol for counters."
  (-add [_ delta] "Add operation in a counter."))
