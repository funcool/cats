(ns cats.monad.continuation
  "The Continuation Monad."
  (:require [cats.protocols :as proto])
  #+clj
  (:require [cats.core :refer [with-context]])
  #+cljs
  (:require-macros [cats.core :refer (with-context)]))

(declare continuation-monad)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type constructors and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype Continuation [mfn]
  proto/Monadic
  (monad [_]
    continuation-monad)

  #+clj   clojure.lang.IFn
  #+cljs  cljs.core/IFn
  (#+clj invoke #+cljs -invoke [self f]
    (mfn f)))

(defn continuation
  "Default constructor for continuation."
  [mfn]
  (Continuation. mfn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Monad definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def continuation-monad
  (reify
    proto/Monad
    (mreturn [_ v]
      (Continuation. (fn [c] (c v))))

    (mbind [_ self mf]
      (Continuation. (fn [c]
                       (self (fn [v]
                               ((mf v) c))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Continuation monad functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-cont
  "Given a Continuation instance, execute the
  wrapped computation and return its value."
  [cont]
  (with-context continuation-monad
    (cont identity)))

(defn call-cc
  [f]
  (continuation
    (fn [cc]
      (let [k (fn [a]
                (continuation (fn [_] (cc a))))]
        ((f k) cc)))))
