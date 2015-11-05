(ns cats.labs.crdt.gset
  "Set union is commutative and convergent; hence it is always safe to
  have simultaneous writes to a set which only allows addition.
  You cannot remove an element of a G-Set."
  (:require [cats.labs.crdt.protocols :as p]
            [cats.protocols :as mp]))

#?(:cljs
   (deftype GSet [s]
     cljs.core/IEquiv
     (-equiv [self other]
       (if (instance? GSet other)
         (= s (.-s other))
         false))

     cljs.core/ICollection
     (-conj [_ o]
       (GSet. (cljs.core/-conj s o)))

     cljs.core/ISeqable
     (-seq [_]
       (cljs.core/-seq s))

     cljs.core/ICounted
     (-count [_]
       (cljs.core/-count s))

     cljs.core/ILookup
     (-lookup [_ v]
       (cljs.core/-lookup s v nil))
     (-lookup [_ v not-found]
       (cljs.core/-lookup s v not-found))

     cljs.core/ISet
     (-disjoin [_ v]
       (throw (ex-info "Operation not allowed" {})))

     cljs.core/IPrintWithWriter
     (-pr-writer [it writer opts]
       (cljs.core/-write writer "#<GSet ")
       (cljs.core/-pr-writer (into '() s) writer opts)
       (cljs.core/-write writer ">"))

     cljs.core/IFn
     (-invoke [coll k]
       (cljs.core/-lookup coll k))
     (-invoke [coll k not-found]
       (cljs.core/-lookup coll k not-found))

     mp/JoinSemiLattice
     (-join [it other]
       (GSet. (into s (.-s other)))))

   :clj
   (deftype GSet [s]
     Object
     (equals [_ other]
       (if (instance? GSet other)
         (= s (.-s other))
         false))

     clojure.lang.IFn
     (invoke [coll k]
       (.get s k))

     clojure.lang.IPersistentSet
     (disjoin [this v]
       (throw (ex-info "The disjoin operation is not allowed" {})))

     (cons [this v]
       (GSet. (conj s v)))

     (empty [this]
       (throw (ex-info "The empty operation is not allowed" {})))

     (equiv [this other]
       (.equals this other))

     (get [_ key]
       (get s key))

     (seq [_]
       (seq s))

     (count [this]
       (count (.-s this)))

     java.io.Serializable

     java.util.Set
     (contains [this k]
       (boolean (get this k)))

     (containsAll [this ks]
       (every? identity (map #(contains? this %) ks)))

     (size [this]
       (count this))

     (isEmpty [this]
       (= 0 (count this)))

     (toArray [this]
       (clojure.lang.RT/seqToArray (seq this)))

     (toArray [this dest]
       (reduce (fn [idx item]
                 (aset dest idx item)
                 (inc idx))
               0, (seq this))
       dest)

     (iterator [_]
       (.iterator s))

     mp/JoinSemiLattice
     (-join [it other]
       (GSet. (into s (.-s other))))))

(defn gset?
  [v]
  (instance? GSet v))

(defn gset*
  [data]
  {:pre [(map? data)
         (:s data)]}
  (if (set? (:s data))
    (GSet. (:s data))
    (GSet. (into #{} (:s data)))))

(defn gset
  "A g-set data type constructor."
  ([]
   (GSet. #{}))
  ([data]
   {:pre [(coll? data)]}
   (if (set? data)
     (GSet. data)
     (GSet. (into #{} data)))))
