(ns cats.test-runner
  (:require [cljs.test :refer-macros [run-tests]]
            [cats.builtin-spec]
            [cats.core-spec]
            [cats.monad.writer-spec]
            [cats.monad.reader-spec]
            [cats.monad.either-spec]
            [cats.monad.maybe-spec]
            [cats.monad.identity-spec]
            [cats.monad.exception-spec]
            [cats.monad.continuation-spec]))

(enable-console-print!)

(defn runner
  []
  (if (cljs.test/successful? (run-tests 'cats.builtin-spec
                                        'cats.core-spec
                                        'cats.monad.continuation-spec
                                        'cats.monad.either-spec
                                        'cats.monad.exception-spec
                                        'cats.monad.maybe-spec
                                        'cats.monad.identity-spec
                                        'cats.monad.reader-spec
                                        'cats.monad.writer-spec))
    0
    1))
