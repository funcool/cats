;; Copyright (c) 2014-2016 Andrey Antukh <niwi@niwi.nz>
;; Copyright (c) 2014-2016 Alejandro Gómez <alejandro@dialelo.com>
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
  "A collection of protocols upon which the cats abstractions are built.

  NOTE: Functions of this namespace are not intended to be used directly.
  It is considered internal api.")

(defprotocol Context
  "A marker protocol for identifying the valid context types.")

(defprotocol Contextual
  "Abstraction that establishes a concrete type as a member of a context.

  A great example is the Maybe monad type Just. It implements
  this abstraction to establish that Just is part of
  the Maybe monad."
  (-get-context [_] "Get the context associated with the type."))

(defprotocol Printable
  "An abstraction to make a type printable in a platform
  independent manner."
  (-repr ^String [_] "Get the repl ready representation of the object."))

(defprotocol Semigroup
  "A structure with an associative binary operation."
  (-mappend [s sv sv'] "An associative addition operation."))

(defprotocol Monoid
  "A Semigroup which has an identity element with respect to an associative binary operation."
  (-mempty [s] "The identity element for the given monoid."))

(defprotocol Extract
  "A type class to extract the
  value from a monad context."
  (-extract [mv] "Extract the value from monad context."))

(defprotocol Functor
  "A data type that can be mapped over without altering its context."
  (-fmap [ftor f fv] "Applies function f to the value(s) inside the context of the functor fv."))

(defprotocol Bifunctor
  "A 'Functor' of two arguments."
  (-bimap  [btor f g bv] "Map over both arguments at the same time."))

(defprotocol Applicative
  "The Applicative abstraction."
  (-fapply [app af av]
    "Applies the function(s) inside af's context to the value(s)
     inside av's context while preserving the context.")
  (-pure [app v]
    "Takes any context or monadic value `app` and any value `v`, and puts
     the value `v` in the most minimal context (normally `mempty`) of same type of `app`"))

(defprotocol Foldable
  "Abstraction of data structures that can be folded to a summary value."
  (-foldl [fctx f z xs] "Left-associative fold of a structure.")
  (-foldr [fctx f z xs] "Right-associative fold of a structure."))

(defprotocol Traversable
  "Abstraction of data structures that can be traversed from left to right
  performing an action on every element."
  (-traverse [tctx f tv]
    "Map each element to an Applicative, evaluate the applicatives from left
     to right and collect the results."))

(defprotocol Monad
  "The Monad abstraction."
  (-mreturn [m v])
  (-mbind [m mv f]))

(defprotocol MonadZero
  "A complement abstraction for monad that
  supports the notion of an identity element."
  (-mzero [m] "The identity element for the given monadzero."))

(defprotocol MonadPlus
  "A complement abstraction for Monad that
  supports the notion of addition."
  (-mplus [m mv mv'] "An associative addition operation."))
