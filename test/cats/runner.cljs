(ns cats.runner
  (:require [clojure.string :as str]
            [cljs.test :as test]
            [cats.core-spec]
            [cats.builtin-spec]
            [cats.monad.exception-spec]
            [cats.monad.continuation-spec]
            [cats.monad.either-spec]
            [cats.monad.maybe-spec]
            [cats.monad.identity-spec]
            [cats.monad.reader-spec]
            [cats.monad.writer-spec]))


(enable-console-print!)

(defn main
  []
  (test/run-tests (test/empty-env)
                  'cats.core-spec
                  'cats.builtin-spec
                  'cats.monad.exception-spec
                  'cats.monad.continuation-spec
                  'cats.monad.either-spec
                  'cats.monad.maybe-spec
                  'cats.monad.identity-spec
                  'cats.monad.reader-spec
                  'cats.monad.writer-spec))

(set! *main-cli-fn* main)
