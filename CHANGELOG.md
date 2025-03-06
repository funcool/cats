# Changelog #

## Version 2.4.3

Date: 2025-03-10

- Change mlet to declare monad functions on a single let form to avoid too long class file names

## Version 2.4.2

Date: 2022-02-22

- Exports clj-kondo configuration, this makes every lib consumer to lint correctly its usages

## Version 2.4.1

Date: 2020-11-04

- Fix bug in `for` implementation

## Version 2.4.0

Date: 2020-11-03

- Add `cats.core/for` macro

## Version 2.3.6

Date: 2020-04-07

- Bump Promesa version from 1.9.0 to 5.1.0

## Version 2.3.5

Date: 2020-02-21

- Change exception monad to catch all throwables instead of exceptions only

## Version 2.3.4

Date: 2020-02-11

- gets can receive monad context as argument

## Version 2.3.3

Date: 2019-10-02

- Fix typo in cats/monad/state.clj: -repr should return a string, not a regex

## Version 2.3.2

Date: 2018-11-07

- Introduce haskell-inspired do-let macro

## Version 2.3.1 ##

Date: 2018-10-31

- Override put,get,swap to receive monad context

## Version 2.3.0 ##

Date: 2018-08-29

- Add state monad implementation

Date: 2018-10-05

- Implement Semigroup for Either

## Version 2.2.0 ##

Date: 2018-01-11

- Fix some issues with wrong handling of dynamic context.
- Convert some functions to macros for delay args evaluation.

## Version 2.1.0 ##

Date: 2017-04-20

- Make cats work together with core.match for some types.
- Convert `when` and `unless` to macros.
- Update promesa dependency.

## Version 2.0.0 ##

Date: 2016-07-28

This version intend to clean the library from unused and out of context stuff
and leave only the useful abstractions. This is a list of main changes (mostly
breaking):

- Removed support for monad transformers. They add too many complex to the
  implementation but in return adds very low value in context of dynamic
  languages as is clojure.
- Removed state, writer, reader and continuation monad namespaces.  They are not
  very useful in clojure so having them without any particular usefulness it not
  make sense.
- Removed `cats.applicative` namespace because it is too many opinionated and
  not really useful in the real world. The use case vary depending on when that
  abstraction is used so we encourage users to define their own abstraction with
  specific behavior for their application.
- Removed CRDT's labs. It was an experiment and the final sensacion is that is
  not very useful and in most circumstances the users defines their own
  datastructures instead of rely on ones from third party packages. So,
  maintaining them in the code base also does not make sense
- Removed `lens` and `traversals` namespaces. The first one becomes very useful
  and is properly externalized as [separated package][1]. The last one, seems
  like it is not very useful and seems out of context of cats library. If anyone
  is interested in maintaining it, the code can be extracted from the previos
  git revisions and released as separated library.
- Move `cats.labs.sugar` into `cats.core` namespace (`cats.labs.sugar` ns is
  removed)

And a list of other changes and contributions:

- PersistentListq support in sequence context (@muhuk)
- Rewrite `cats.labs.channel` integration with core.async combinators
  (@yanatan16)
- Add `cats.builtin.lazy-sequence-context` (@muhuk)
- Fix bug in sequence context's fapply implementation (@muhuk)
- Performance improvements in sequence context implementation (@muhuk)
- Add `cats.monad.either/try-either` macro for capturing exceptions as left, values
  as right (@shmish111)
- Move `promesa.monad` namespace under `cats.labs.promise` ns.

[1]: https://github.com/funcool/lentes


## Version 1.2.1 ##

Date: 2015-12-16

- Fix functor implementation for Map.
- Fix key overwritting on lenses focus/foci types.
- Add utility functions to validation ns.


## Version 1.2.0 ##

Date: 2015-12-01

- Add Bifunctor (@yurrriq)
- Add a macro for lifting functions to applicatives

## Version 1.1.0 ##

Date: 2015-11-26

- Add Lenses and Traversals.
- Add timeout support for manifold deferred context.
- Improved mapseq implementation.
- Added function monoid and monad.
- Implement functor, monad and foldable for any kind of map.
- Make all types printable.
- Add monadzero and monadplus for either and either-t.
- Add `cats.labs.sugar` namespace with sugar syntax macros
  (documentation is very welcome).
- Update core.async version to 0.2.374
- Update cljs compiler version to 0.7.170
- Add basic support for crdt's under labs namespace.
(documentation is missing).
- Add test.check generators integration and property constructors


## Version 1.0.0 ##

Date: 2015-09-17

### Bug fixes

- The context doesn't need to be a monad anymore.
- Fix Foldable implementation for clojure builtin collections.

### New features

- Add additional arity to `cats.core/mempty` function, that allows provide a context
  instead of resolving it.
- Sorted map is now also implements monoid.
- Add `cats.labs` namespace for make room for previously removed **reader**, **writer**,
  **state** and **continuation** monads.
- Add core.async channel monad/applicative under `cats.labs.channel` namespace.
- Add manifold deferred monad/applicative under `cats.labs.manifold` namespace.
- Add **applicative-do** (with `alet` macro) syntax. You can read more about that in
  [haskell wiki](https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo) and
  [pull request](https://github.com/funcool/cats/pull/63).
- Add **foldm** implementation to `cats.core` namespace.
- Add a bunch of monoids: `all`, `any`, `sum`, `prod`, `pair` and `string`.
- Add a [Traversable](https://hackage.haskell.org/package/base-4.8.1.0/docs/Data-Traversable.html)
  protocol and implementations for vector, sequence, pair, maybe, either and validation.
- Add **traverse** function to `cats.core` namespace.

### Backwards incompatible changes

- Revisit monad-transformers abstraction removing useless protocol methods: `base` and `inner`
  are no longer part of the `MonadTrans` protocol.
- The protocols methods are all renamed to `-name` for consistency with cljs style to
  name internal functions.
- The `Context` protocol has been renamed to `Contextual`, and `Context` is now a protocol
  used to mark context instances and resolve their priority.


## Version 0.6.1 ##

Date: 2015-08-02

- Remove src/user.clj that causes some problems.


## Version 0.6.0 ##

Date: 2015-08-02

- Foldable abstraction.
- Curry facilities.
- Major code cleaning.
- Context management moved into its own namespace.
- A lot of performance improvements (removing unnecessary
  function call forwarding).
- Start using the clojurescript 1.7.28.


## Version 0.5.0 ##

Date: 2015-07-13

- Set default and required clojure version to 1.7.0.
- Replace cljx with clojure 1.7 conditional reader.
- Add more util functions for treat for maybe monad types.
- Add more util functions for treat for either monad types.
- Add semigroup and monoid abstractions.
- Add validation applicative (similar to either).
- Minor performance improvements.
- Fix inconsistences between fmap / fapply public functions
  and its haskell-style aliases.
- Add more documentation.
- Deprecate the state, reader, writer and continuation
  monads. They will be moved to a separated library for
  future version.
- Remove already deprecated `from-either` function.


## Version 0.4.0 ##

Date: 2015-03-25

- The exception monad now raises a exception when you deref a failure instance.
  This will allow easy use of monadic api from libraries or applications that
  does not  use monads.
- The `cats.monad.exception/failure` function now coerce all input that not
  extends from exception to exception type (using `ex-info` clojure function).
- Add channel monad using core.async channels abstraction (as separated repository)
  This make easy treat channels as monads and using them in monadic compositions.
- Remove mlet-with macro (bacause it sintax is ugly and with-monad usage is preferable)
- Remove undocumented errlet macro.


## Version 0.3.4 ##

Date: 2015-03-14

- Improve cljx build process.


## Version 0.3.3 ##

Date: 2015-03-14

- Improvements on project.clj.


## Version 0.3.2 ##

Date: 2015-02-08

- Empty version, only exists because clojars does not
  allow remove wrong uploaded versions.


## Version 0.3.0 ##

Date: 2015-02-08

- Dependencies versions update.
- Add new arity to from-maybe function for default value.
- Update required clojurescript version to 0.0-2760.
- Use speclj for tests.
- Add IDeref implementation to either, maybe, identity and exception monads types.
- Remove maybe useless lazy monad.

## Version 0.2.0 ##

Date: 2014-10-26

- Major api redesign.
- Split monad definition from data type.
- mlet macro is now not recursive.
- New monads: lazy, exception.
- Monad transformers.

## Version 0.1.0 ##

Date: 2014-07-09

- First relase.
