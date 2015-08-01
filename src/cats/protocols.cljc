;; Copyright (c) 2014-2015 Andrey Antukh <niwi@niwi.nz>
;; Copyright (c) 2014-2015 Alejandro Gómez <alejandro@dialelo.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(ns cats.protocols
  "Abstractions of Category Theory over that
  cats library is build.

  Functions of this namespace are not indented
  to be used directly. Is a private api but exposes
  as public for documentation purposes.")

(defprotocol Context
  "Abstraction that establish a membership of types
  with one concrete monad.

  This is a way that cats establishes the relation
  between a type and the monad that that should play.

  A great example es Maybe monad type Just. It implements
  this abstraction for establish that Just is part of
  Maybe monad."
  (get-context [_] "Get the monad instance for curent value."))

(defprotocol Semigroup
  "A structure with an associative binary operation."
  (mappend [s sv sv'] "An associative addition operation."))

(defprotocol Monoid
  "A Semigroup which has an identity element for is associative binary operation."
  (mempty [s] "The identity element for the given monoid."))

(defprotocol Extract
  "A type class responsible of extract the
  value from a monad context."
  (extract [mv] "Extract the value from monad context."))

(defprotocol Functor
  "A data type that can be mapped over without altering its context."
  (fmap [ftor f fv] "Applies function f to the value(s) inside the context of the functor fv."))

(defprotocol Applicative
  "The Applicative abstraction."
  (fapply [app af av]
    "Applies the function(s) inside ag's context to the value(s)
     inside av's context while preserving the context.")
  (pure [app v]
    "Takes any context monadic value ctx and any value v, and puts
     the value v in the most minimal context of same type of ctx"))

(defprotocol Foldable
  "Abstraction of data structures that can be folded to a summary value."
  (foldl [fctx f z xs] "Left-associative fold of a structure.")
  (foldr [fctx f z xs] "Right-associative fold of a structure."))

(defprotocol Monad
  "The Monad abstraction."
  (mreturn [m v])
  (mbind [m mv f]))

(defprotocol MonadZero
  "A complement abstraction for monad that
  supports the notion of an identity element."
  (mzero [m] "The identity element for the given monadzero."))

(defprotocol MonadPlus
  "A complement abstraction for Monad that
  supports the notion of addition."
  (mplus [m mv mv'] "An associative addition operation."))

(defprotocol MonadState
  "A specific case of Monad abstraction for
  work with state in pure functional way."
  (get-state [m] "Return the current state.")
  (put-state [m newstate] "Update the state.")
  (swap-state [m f] "Apply a function to the current state and update it."))

(defprotocol MonadReader
  "A specific case of Monad abstraction that
  allows a read only access to an environment."
  (ask [m] "Return the current environment.")
  (local [m f reader] "Create a reader in a modified version of the environment."))

(defprotocol MonadWriter
  "A specific case of Monad abstraction that
  allows emulate write operations in pure functional
  way.

  A great example is writing a log message."
  (listen [m mv] "Given a writer, yield a (value, log) pair as a value.")
  (tell [m v] "Add the given value to the log.")
  (pass [m mv]
    "Given a writer whose value is a pair with a function as its second element,
     yield a writer which has the first element of the pair as the value and
     the result of applying the aforementioned function to the log as the new log."))

(defprotocol MonadTrans
  "A common abstraction for all monad transformers."
  (base [mt] "Return the base monad of this transformer.")
  (inner [mt] "Return the monad that this transformer wraps.")
  (lift [m mv]
    "Lift a value from the parameterized monad to the transformer."))
