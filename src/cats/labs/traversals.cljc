(ns cats.labs.traversals
  (:require [cats.util :as util]
            [cats.protocols :as p]
            [cats.labs.lens :as l])
  (:refer-clojure :exclude [nth key keys vals filter select-keys cat]))

(defn traversal
  "Given a function for getting the focused values from a state (getter)
  and a function that takes the state and and update function (setter),
  constructs a traversal."
  [getter setter]
  (fn [next]
    (fn
      ([s]
       (mapcat next (getter s)))
      ([s f]
       (setter s #(next % f))))))

(def it
  "Identity traversal."
  (traversal list l/id-setter))

(def nothing
  "Identity traversal under `both`."
  (traversal
   (constantly [])
   l/const-setter))

(defn foci
  "Given a traversal and a state, return the values focused by the traversal."
  [traversal s]
  (let [getter (traversal list)]
    (getter s)))

(defn ffoci
  "Given a traversal and a state, return the first value focused by the traversal."
  [traversal s]
  (first (foci traversal s)))

(defn both
  "Given two traversals, compose them in parallel. The getter of
  the resulting traversal will combine results from both traversals
  and the setter will update the state with both setters."
  [one other]
  (traversal
   (fn [s]
     (concat (foci one s)
             (foci other s)))
   (fn [s f]
     (->> s
          (l/over one f)
          (l/over other f)))))

(defn lens->traversal
  "Derive a traversal from a lens."
  [l]
  (traversal
   (fn [s]
     (list (l/focus l s)))
   (fn [s f]
     (l/over l f s))))

(def each
  "A traversal into each element of a sequence."
  (traversal
   sequence
   (fn [s f]
     (into (l/sequential-empty s) (map f s)))))

(def cat
  (comp each each))

(defn filter
  "Given a predicate, return a traversal that focuses the elements that
  pass the predicate."
  [applies?]
  (traversal
   (fn [s]
     (if (applies? s)
       [s]
       []))
   (fn [s f]
     (if (applies? s)
       (f s)
       s))))

(defn only
  "Given a predicate, return a traversal that filters a coll."
  [applies?]
  (comp each (filter applies?)))

(def keys
  "Focus on the keys of an associative data structure."
  (traversal
   (fn [s]
     (clojure.core/keys s))
   (fn [s f]
     (zipmap (map f (clojure.core/keys s))
             (clojure.core/vals s)))))

(def vals
  "Focus on the values of an associative data structure."
  (traversal
   (fn [s]
     (clojure.core/vals s))
   (fn [s f]
     (zipmap (clojure.core/keys s)
             (map f (clojure.core/vals s))))))

(def indexed
  (traversal
   (fn [s]
     (map-indexed vector s))
   (fn [s f]
     (map f (map-indexed vector s)))))

(deftype Foci [id trav a]
  p/Printable
  (-repr [m]
    (str "#<Foci [" (pr-str trav) "," (pr-str a) "]>"))

  #?@(:clj
      [clojure.lang.IDeref
       (deref [_] (foci trav @a))

       clojure.lang.IAtom
       (reset [self newval]
         (swap! a #(l/put trav newval %))
         (deref self))
       (swap [self f]
         (swap! a (fn [s] (l/over trav f s)))
         (deref self))
       (swap [self f x]
         (swap! a (fn [s] (l/over trav #(f % x) s)))
         (deref self))
       (swap [self f x y]
         (swap! a (fn [s] (l/over trav #(f % x y) s)))
         (deref self))
       (swap [self f x y more]
         (swap! a (fn [s] (l/over trav #(apply f % x y more) s)))
         (deref self))

       clojure.lang.IRef
       (addWatch [self key cb]
         (let [key (l/prefix-key key id)]
           (add-watch a key (fn [key _ oldval newval]
                              (let [old' (foci trav oldval)
                                    new' (foci trav newval)]
                                (if (not= old' new')
                                  (cb key self old' new')))))))
       (removeWatch [_ key]
         (let [key (l/prefix-key key id)]
           (remove-watch a key)))]

      :cljs
      [IDeref
       (-deref [_] (foci trav @a))

       IWatchable
       (-add-watch [self key cb]
         (let [key (l/prefix-key key id)]
           (add-watch a key (fn [key _ oldval newval]
                              (let [old' (foci trav oldval)
                                    new' (foci trav newval)]
                                (if (not= old' new')
                                  (cb key self old' new')))))))
       (-remove-watch [_ key]
         (let [key (l/prefix-key key id)]
           (remove-watch a key)))

       IReset
       (-reset! [self newval]
         (swap! a #(l/put trav newval %))
         (deref self))

       ISwap
       (-swap! [self f]
        (swap! a (fn [s] (l/over trav f s)))
        (deref self))

       (-swap! [self f x]
         (swap! a (fn [s] (l/over trav #(f % x) s)))
         (deref self))

       (-swap! [self f x y]
         (swap! a (fn [s] (l/over trav #(f % x y) s)))
         (deref self))

       (-swap! [self f x y more]
         (swap! a (fn [s] (l/over trav #(apply f % x y more) s)))
         (deref self))]))

(defn foci-atom
  [traversal a]
  (let [id (str (gensym "cats-traversals"))]
    (Foci. id traversal a)))

(util/make-printable Foci)
