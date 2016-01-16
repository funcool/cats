(ns cats.monad
  (:require [cats.protocols :as p]))

;;

(defprotocol Computation
  (-run [computation context])
  (-step? [computation]))

(defn mrun
  [computation context]
  (let [nxt (-run computation context)]
    (if (-step? nxt)
      (recur nxt context)
      nxt)))

;;

(deftype Apply [f vs]
  Computation
  (-run [_ ctx]
    (transduce (map #(-run % ctx))
               (completing
                (fn [f v]
                  (p/-fapply ctx f v)))
               (-run f ctx)
               vs))

  (-step? [_] true))

(defn fapply
  [f & vs]
  (Apply. f vs))

(deftype Return [x]
  Computation
  (-run [_ ctx]
    (p/-mreturn ctx x))

  (-step? [_] true))

(defn return
  [x]
  (Return. x))

(deftype Bind [m k]
  Computation
  (-run [_ ctx]
    (p/-mbind ctx
              (mrun m ctx)
              (comp #(mrun % ctx) k)))

  (-step? [_] true))

(defn bind
  [m k]
  (Bind. m k))

(deftype Zero []
  Computation
  (-run [_ ctx]
    (p/-mzero ctx))

  (-step? [_] true))

(defn mzero
  []
  (Zero.))

(deftype Plus [a b]
  Computation
  (-run [_ ctx]
    (p/-mplus ctx a b))

  (-step? [_] true))

(defn mplus
  [a b]
  (Plus. a b))

;;

(extend-protocol Computation
  #?(:clj Object :cljs object)
  (-run [o _] o)
  (-step? [_] false)

  nil
  (-run [_ _] nil)
  (-step? [_] false))

;; macros

(defmacro mdo
  [bindings & body]
  (when-not (and (vector? bindings)
                    (not-empty bindings)
                    (even? (count bindings)))
    (throw (IllegalArgumentException. "bindings has to be a vector with even number of elements.")))
  (->> (reverse (partition 2 bindings))
       (reduce (fn [acc [l r]]
                 (case l
                   :let  `(let ~r ~acc)
                   :when `(Bind. (if ~r
                                   (Return. nil)
                                   (Zero.))
                                 (fn [~(gensym)] ~acc))
                   `(Bind. ~r (fn [~l] ~acc))))
               `(do ~@body))))

(comment
  (require '[cats.builtin :as b])
  (require '[cats.monad.maybe :as maybe])

  (mrun (return 42) b/sequence-context)
  (mrun (return 42) maybe/context)

  (mrun (bind (return 42)
              (fn [x] (return (inc x))))
        maybe/context)

  (mrun (bind (return 42) (fn [x] (return (inc x))))
        b/sequence-context)

  (mrun (mdo [x (return 2)
              y (return 21)]
           (return (* x y)))
        maybe/context)

  (mrun (mdo [x (return 2)
              y (return 21)]
             (return (* x y)))
        b/sequence-context)

  (mrun (mdo [x [1 2 3]
              y [4 5 6]
              :let [z (+ x y)]
              :when (even? z)]
             (return z))
        b/sequence-context)

  (mrun
   (fapply (return inc) (return 2))
   maybe/context)

  (mrun
   (fapply (fapply (return (fn [x]
                             (fn [y]
                               (* x y))))
                   (return 2))
           (return 21))
   maybe/context)

  (mrun
   (fapply (return (fn [x]
                     (fn [y]
                       (* x y))))
            (return 2)
            (return 21))
   maybe/context)

  ;;

  (require '[cats.core :as core])
  (require '[cats.context :as ctx])

  (defmacro expand
    [n]
    (let [bindings (interleave (repeatedly n gensym)
                               (repeat `(core/return 1)))]
      (println :bindings bindings)
      `(core/mlet [~@bindings]
         (core/return 42))))

  ;; 1000
  (ctx/with-context maybe/context
    (expand 1e3))
  
  (clojure.pprint/pprint
   (clojure.walk/macroexpand-all '(expand 10)))

  
  ;; FIXME
  (mrun
   (last
    (take 1e3
          (iterate (fn [computation]
                     (Bind. computation (fn [x]
                                          (Return. (inc x)))))
                   (Return. 0))))
   
   maybe/context)
  )





