{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Reference where

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

runReference :: Reference a -> a
runReference (RPrim a) = a
runReference _ = error "Not a prim!"
