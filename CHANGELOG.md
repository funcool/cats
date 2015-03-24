# Changelog #

## Version 0.4.0 ##

Date: unreleased

- The exception monad now raises a exception when you deref a failure instance.
  This will allow easy use of monadic api from libraries or applications that
  does not  use monads.
- The `cats.monad.exception/failure` function now coerce all input that not
  extends from exception to exception type (using `ex-info` clojure function).
- Add channel monad using core.async channels abstraction.
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
