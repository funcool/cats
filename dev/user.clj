(ns user
  (:require [clojure.tools.namespace.repl :as repl]
            [clojure.test :as test]))

(defonce ^:dynamic
  *namespaces*
  ['cats.core-spec
   'cats.builtin-spec
   'cats.applicative.validation-spec
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
      (repl/refresh :after 'user/run-tests'))
    (repl/refresh :after 'user/run-tests')))
