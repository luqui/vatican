{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

module HOAS (Primitive(..), Term(..), PrimTerm(..), let_) where

class (Show a) => Primitive a where
    apply :: a -> a -> a

infixl 9 %
class Term t where
    (%) :: t -> t -> t
    fun :: (t -> t) -> t

let_ :: (Term t) => t -> (t -> t) -> t
let_ defn body = fun body % defn

class (Term t) => PrimTerm a t | t -> a where
    prim :: a -> t
