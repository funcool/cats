(ns cats.runner
  (:require [clojure.string :as str]
            [cljs.test :as test]
            [cats.core-spec]
            [cats.builtin-spec]
            [cats.applicative.validation]
            [cats.monad.exception-spec]
            [cats.monad.either-spec]
            [cats.monad.maybe-spec]
            [cats.monad.identity-spec]
            [cats.labs.sugar-spec]
            [cats.labs.continuation-spec]
            [cats.labs.channel-spec]
            [cats.labs.state-spec]
            [cats.labs.reader-spec]
            [cats.labs.writer-spec]
            [cats.labs.crdt.pncounter-spec]
            [cats.labs.crdt.gcounter-spec]
            [cats.labs.crdt.gset-spec]
            [cats.labs.lens-spec]))

(enable-console-print!)

(defn -main []
  (test/run-tests
   (test/empty-env)
   'cats.core-spec
   'cats.builtin-spec
   'cats.applicative.validation
   'cats.monad.exception-spec
   'cats.monad.either-spec
   'cats.monad.maybe-spec
   'cats.monad.identity-spec
   'cats.labs.sugar-spec
   'cats.labs.continuation-spec
   'cats.labs.state-spec
   'cats.labs.reader-spec
   'cats.labs.writer-spec
   'cats.labs.channel-spec
   'cats.labs.crdt.pncounter-spec
   'cats.labs.crdt.gcounter-spec
   'cats.labs.crdt.gset-spec
   'cats.labs.lens-spec))

(defmethod test/report [:cljs.test/default :end-run-tests] [m]
  (when-not (test/successful? m)
    ((aget js/process "exit") 1)))

(set! *main-cli-fn* -main)
