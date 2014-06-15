(ns cats.protocols
  "Monadic types definition.")

(defprotocol Monad
  "Incomplete monad type definition."
  (bind [mv f] "Applies the function f to the value(s) inside mv's context."))

(defprotocol MonadZero
  "A `Monad` that supports the notion of an identity element."
  (mzero [ctx] "The identity element for `ctx`."))

(defprotocol MonadPlus
  "A `MonadZero` that supports the notion of addition."
  (mplus [mv mv'] "An associative addition operation."))

(defprotocol Functor
  (fmap [fv f] "Applies function f to the value(s) inside the context of the functor fv."))

(defprotocol Applicative
  (fapply [af av]
    "Applies the function(s) inside ag's context to the value(s)
     inside av's context while preserving the context.")
  (pure [ctx v]
    "Takes any context monadic value ctx and any value v, and puts
     the value v in the most minimal context of same type of ctx"))
