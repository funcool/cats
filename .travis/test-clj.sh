#!/bin/bash

# FIXME: Explicit namespace are needed because Leiningen ignores .cljc files :_
#  https://github.com/technomancy/leiningen/issues/1827
TEST_NAMESPACES=(
    cats.core-spec
    cats.data-spec
    cats.builtin-spec
    cats.applicative.validation-spec
    cats.labs.channel-spec
    cats.labs.manifold-spec
    cats.labs.state-spec
    cats.labs.reader-spec
    cats.labs.writer-spec
    cats.labs.continuation
    cats.monad.exception-spec
    cats.monad.either-spec
    cats.monad.maybe-spec
    cats.monad.identity-spec
)

lein2 test ${TEST_NAMESPACES[@]}
