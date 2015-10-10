(ns cats.labs.crdt.protocols
  (:require [cats.labs.crdt.util :as u]))

(defprotocol ICounter
  "A basic protocol for counters."
  (-add [_ delta] "Add operation in a counter."))
