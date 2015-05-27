#!/bin/bash

# FIXME: Explicit namespace are needed because Leiningen ignores .cljc files :_
#  https://github.com/technomancy/leiningen/issues/1827
TEST_NAMESPACES=(
    cats.core-spec
    cats.builtin-spec
    cats.monad.exception-spec
    cats.monad.continuation-spec
    cats.monad.either-spec
    cats.monad.maybe-spec
    cats.monad.identity-spec
    cats.monad.reader-spec
    cats.monad.writer-spec
)

lein2 test ${TEST_NAMESPACES[@]}
