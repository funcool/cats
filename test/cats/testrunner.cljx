#+cljs
(ns cats.testrunner
  (:require [cljs-testrunners.node :as node]
            [cats.builtin-spec]
            [cats.core-spec]
            [cats.monad.writer-spec]
            [cats.monad.reader-spec]
            [cats.monad.either-spec]
            [cats.monad.maybe-spec]
            [cats.monad.identity-spec]
            [cats.monad.exception-spec]
            [cats.monad.continuation-spec]))

#+cljs
(defn main []
  (node/run-tests
   'cats.core-spec
   'cats.builtin-spec
   'cats.monad.exception-spec
   'cats.monad.continuation-spec
   'cats.monad.either-spec
   'cats.monad.maybe-spec
   'cats.monad.identity-spec
   'cats.monad.reader-spec
   'cats.monad.writer-spec
   ))

#+cljs
(set! *main-cli-fn* main)
