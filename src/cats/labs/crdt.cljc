(ns cats.labs.crdt
  (:refer-clojure :exclude [+ inc val merge dec])
  (:require [cats.labs.crdt.protocols :as p]
            [cats.labs.crdt.gcounter]
            [cats.labs.crdt.pncounter]
            [cats.protocols :as mp]
            #?(:cljs [cljs.reader :as edn]
               :clj [clojure.edn :as edn])))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Serialization Impl and Api
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprotocol ISerializable
  (-dump [_] "Get plain representation of the data type."))

(extend-protocol ISerializable
  cats.labs.crdt.gcounter.GCounter
  (-dump [it]
    {:type ::gcounter :e (.-e it)})

  cats.labs.crdt.pncounter.PNCounter
  (-dump [it]
    {:type ::pncounter
     :p (-dump (.-p it))
     :n (-dump (.-n it))}))

(defmulti -load :type)
(defmulti -encode (fn [_ type] type))
(defmulti -decode (fn [_ type] type))

(defmethod -encode :edn
  [data _]
  (pr-str (-dump data)))

(defmethod -decode :edn
  [data _]
  (let [data (edn/read-string data)]
    (-load data)))

(defmethod -load ::gcounter
  [data]
  (cats.labs.crdt.gcounter/gcounter* data p/*node*))

(defmethod -load ::pncounter
  [data]
  (cats.labs.crdt.pncounter/pncounter* data p/*node*))

;; Public Api

(defn encode
  "Serialize crdt data type.

  Only edn format is supported out
  of the box. This api is extensible."
  ([data]
   (encode data :edn))
  ([data format]
   (-encode data format)))

(defn decode
  "Deserialize crdt data type.

  Only edn format is supported out
  of the box. This apo is extensible."
  ([data]
   (decode data :edn))
  ([data format]
   (-decode data :edn)))
