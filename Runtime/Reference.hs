{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

-- A direct interpretation of HOAS in Haskell, for sanity checking the results
-- of interpreter stacks.

module Runtime.Reference (Reference, eval) where

import HOAS

data Reference a
    = RPrim a
    | RFun (Reference a -> Reference a)

instance (Primitive a) => Term (Reference a) where
    RPrim a % RPrim b = RPrim (a `apply` b)
    RPrim a % RFun _  = error "Type error!"
    RFun  f % b       = f b
    fun = RFun

instance (Primitive a) => PrimTerm a (Reference a) where
    prim = RPrim

eval :: Reference a -> a
eval (RPrim a) = a
eval _ = error "Not a prim!"
