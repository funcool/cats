(ns cats.labs.lens
  (:require
   [cats.util :as util]
   [cats.protocols :as p])
  (:refer-clojure :exclude [nth key keys vals filter select-keys cat]))

;; constructors

(defn lens
  "Given a function for getting the focused value from a state (getter)
  and a function that takes the state and and update function (setter),
  constructs a lens."
  [getter setter]
  (fn [next]
    (fn
      ([s]
        (next (getter s)))
      ([s f]
        (setter s #(next % f))))))

;; base lenses

(defn id-setter
  "The identity setter, applies the function to the state."
  [s f]
  (f s))

(defn const-setter
  "The constant setter, returns the state unaltered."
  [s _]
  s)

(def id
  "Identity lens."
  (lens identity id-setter))

;; API

(defn focus
  "Given a lens and a state, return the value focused by the lens."
  [lens s]
  (let [getter (lens identity)]
    (getter s)))

(defn over
  "Given a setter, a function and a state, apply the function over
  the value focused by the setter."
  [st f s]
  (let [setter (st id-setter)]
    (setter s f)))

(defn put
  "Given a setter, a new value and a state, replace the value focused by
  the lens with the new one."
  [st v s]
  (over st (constantly v) s))

;; combinators

(defn units
  "Given a function from unit A to unit B and another in the
  opposite direction, construct a lens that focuses and updates
  a converted value."
  [one->other other->one]
  (lens
   one->other
   (fn [s f]
     (other->one (f (one->other s))))))

;; lenses

(defn passes
  "Given a predicate, return a lens that focuses in an element only
  if passes the predicate.

  The lens is not well-behaved, depens on the outcome of the predicate."
  [applies?]
  (lens
   (fn [s]
     (when (applies? s)
       s))
   (fn [s f]
     (if (applies? s)
       (f s)
       s))))

(defn nth
  "Given a number, returns a lens that focuses on the given index of
  a collection."
  [n]
  (lens
    (fn [s]
      (clojure.core/nth s n))
    (fn [s f]
      (update s n f))))

(def fst (nth 0))
(def snd (nth 1))

(defn sequential-empty
  {:internal true
   :no-doc true}
  [coll]
  (cond
    (map? coll) {}
    (set? coll) #{}
    :else []))

(def tail
  "A lens into the tail of a collection."
  (lens
   rest
   (fn [s f]
     (into (sequential-empty s)
           (cons (first s)
                 (f (rest s)))))))

(defn key
  "Given a key, returns a lens that focuses on the given key of
  an associative data structure."
  [k]
  (lens
   (fn [s]
     (get s k))
   (fn [s f]
     (update s k f))))

(defn select-keys
  "Return a lens focused on the given keys in an associative data
  structure."
  [ks]
  (lens
   (fn [s]
     (clojure.core/select-keys s ks))
   (fn [s f]
     (merge (apply dissoc s ks)
            (-> (clojure.core/select-keys s ks)
                f
                (clojure.core/select-keys ks))))))

(defn in
  "Given a path and optionally a default value, return a lens that
  focuses the given path in an associative data structure."
  ([path]
   (in path nil))
  ([path default]
   (lens
    (fn [s]
      (get-in s path default))
    (fn [s f]
      (update-in s path f)))))

;; interop

(defn prefix-key
  {:internal true
   :no-doc true}
  [key id]
  (keyword (str id "-" (name key))))

(deftype Focus [id lens a]
  p/Printable
  (-repr [m]
    (str "#<Focus [" (pr-str lens) "," (pr-str a) "]>"))

  #?@(:clj
      [clojure.lang.IDeref
       (deref [_] (focus lens @a))

       clojure.lang.IAtom
       (reset [self newval]
         (swap! a #(put lens newval %))
         (deref self))
       (swap [self f]
         (swap! a (fn [s] (over lens f s)))
         (deref self))
       (swap [self f x]
         (swap! a (fn [s] (over lens #(f % x) s)))
         (deref self))
       (swap [self f x y]
         (swap! a (fn [s] (over lens #(f % x y) s)))
         (deref self))
       (swap [self f x y more]
         (swap! a (fn [s] (over lens #(apply f % x y more) s)))
         (deref self))

       clojure.lang.IRef
       (addWatch [self key cb]
         (let [key (prefix-key key id)]
           (add-watch a key (fn [key _ oldval newval]
                              (let [old' (focus lens oldval)
                                    new' (focus lens newval)]
                                (if (not= old' new')
                                  (cb key self old' new')))))))
       (removeWatch [_ key]
         (let [key (prefix-key key id)]
           (remove-watch a key)))]

      :cljs
      [IDeref
       (-deref [_] (focus lens @a))

       IWatchable
       (-add-watch [self key cb]
         (let [key (prefix-key key id)]
           (add-watch a key (fn [key _ oldval newval]
                              (let [old' (focus lens oldval)
                                    new' (focus lens newval)]
                                (if (not= old' new')
                                  (cb key self old' new')))))))
       (-remove-watch [_ key]
         (let [key (prefix-key key id)]
           (remove-watch a key)))

       IReset
       (-reset! [self newval]
         (swap! a #(put lens newval %))
         (deref self))

       ISwap
       (-swap! [self f]
        (swap! a (fn [s] (over lens f s)))
        (deref self))

       (-swap! [self f x]
         (swap! a (fn [s] (over lens #(f % x) s)))
         (deref self))

       (-swap! [self f x y]
         (swap! a (fn [s] (over lens #(f % x y) s)))
         (deref self))

       (-swap! [self f x y more]
         (swap! a (fn [s] (over lens #(apply f % x y more) s)))
         (deref self))]))

(defn focus-atom
  [lens a]
  (let [id (str (gensym "cats-lens"))]
    (Focus. id lens a)))

(util/make-printable Focus)
