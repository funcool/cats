(ns cats.protocols
  "Cathegory theory types definition.")

(defprotocol Functor
  (fmap [ftor f fv] "Applies function f to the value(s) inside the context of the functor fv."))

(defprotocol Applicative
  (fapply [app af av]
    "Applies the function(s) inside ag's context to the value(s)
     inside av's context while preserving the context.")
  (pure [app v]
    "Takes any context monadic value ctx and any value v, and puts
     the value v in the most minimal context of same type of ctx"))

(defprotocol Monad
  (mreturn [m v])
  (mbind [m mv f]))

(defprotocol Monadic
  "A type that is part of a monad."
  (monad [_] ))

(defprotocol MonadZero
  "A `Monad` that supports the notion of an identity element."
  (mzero [m] "The identity element for `ctx`."))

(defprotocol MonadPlus
  "A `MonadZero` that supports the notion of addition."
  (mplus [m mv mv'] "An associative addition operation."))

(defprotocol MonadTrans
  (inner [mt] "Return the inner monad of this transformer.")
  (lift [m mv] "Lift a value from the inner monad to the outher monad."))
