(ns user
  (:require [clojure.tools.namespace.repl :refer [refresh]]
            [clojure.test :refer [run-tests]])
  (:refer-clojure :exclude [test]))

;; (defn test
;;   ([]
;;    (refresh)
;;    (run-tests 'test-core
;;               'test-builtin
;;               'test-maybe
;;               'test-either
;;               'test-state
;;               'test-continuation))
;;   ([& namespaces]
;;    (apply run-tests namespaces)))
