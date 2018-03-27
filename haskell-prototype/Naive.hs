{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

-- A naive, lazy interpreter. It has a terrible constant overhead,
-- but, perhaps surprisingly, it passes the tower of interpreters
-- test.

module Naive where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IntSet (IntSet)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.IntSet as I
import qualified Data.Supply as Supply

import HOAS

data Exp a = Var Int
           | Lam Int (Exp a)
           | Exp a `App` Exp a
           | Prim a
           deriving Show

newtype Env a = Env { runEnv :: Supply.Supply Int -> a }

instance Functor Env where
    fmap f (Env s) = Env (f . s)

instance Applicative Env where
    pure = return
    (<*>) = ap

instance Monad Env where
    return = Env . const
    Env s >>= f = Env $ \sup ->
        let (sup1, sup2) = Supply.split2 sup in
        runEnv (f (s sup1)) sup2

newtype Naive a = Naive { unNaive :: Env (Exp a) }

fresh :: Env Int
fresh = Env Supply.supplyValue

instance Term (Naive a) where
  Naive left % Naive right = Naive $ liftM2 App left right
  fun f = Naive $ do
    x <- fresh
    Lam x `liftM` (unNaive . f . Naive . return $ Var x)

instance PrimTerm a (Naive a) where
  prim = Naive . return . Prim

freeVars :: Exp a -> IntSet
freeVars (Var v) = I.singleton v
freeVars (Lam v e) = I.delete v $ freeVars e
freeVars (App f a) = freeVars f `I.union` freeVars a
freeVars _ = I.empty

subst :: Int -> Exp a -> Exp a -> Env (Exp a)
subst x s b = sub b
  where sub e@(Var v) | v == x = return s
                      | otherwise = return e
        sub e@(Lam v e') | v == x = return e
                         | v `I.member` fvs = do
                             v' <- fresh
                             e'' <- sub =<< subst v (Var v') e'
                             return $ Lam v' e''
                         | otherwise = Lam v `liftM` sub e'
        sub (App f a) = liftM2 App (sub f) (sub a)
        sub e = return e
        fvs = freeVars s

reduce :: Primitive a => Exp a -> Env (Exp a)
reduce (Lam x e) = Lam x `liftM` reduce e
reduce (App e1 e2) = do
  e1' <- reduce e1
  e2' <- reduce e2
  case e1' of
    Lam x e -> reduce =<< subst x e2' e
    Prim a -> case e2' of
      Prim b -> return . Prim $ a `apply` b
      _ -> return $ App e1' e2'
    _ -> return $ App e1' e2'
reduce e = return e

eval :: Primitive a => Naive a -> a
eval m = case e of
  Prim a -> a
  _ -> error "Not a prim!"
  where 
  e = unsafePerformIO $ do
        supply <- Supply.newSupply 0 succ
        return $ runEnv (reduce =<< unNaive m) supply
