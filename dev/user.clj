(ns user
  (:require [clojure.tools.namespace.repl :as r]
            [clojure.walk :refer [macroexpand-all]]
            [clojure.pprint :refer [pprint]]
            [clojure.test :as test]))

(defonce ^:dynamic
  *namespaces*
  ['cats.core-spec
   'cats.builtin-spec
   'cats.labs.channel-spec
   'cats.monad.identity-spec
   'cats.monad.either-spec
   'cats.monad.exception-spec
   'cats.monad.maybe-spec])

(defn run-tests'
  []
  (apply test/run-tests *namespaces*))

(defn run-tests
  [& nss]
  (if (pos? (count nss))
    (binding [*namespaces* nss]
      (r/refresh :after 'user/run-tests'))
    (r/refresh :after 'user/run-tests')))

(defn trace
  "Asynchronous friendly println variant."
  [& strings]
  (locking println
    (apply println strings)))

;; (require '[cats.core :as m]
;;          '[cats.context :as mc]
;;          '[cats.monad.either :as either]
;;          '[cats.builtin])
