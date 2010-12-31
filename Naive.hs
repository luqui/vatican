{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}

-- A naive, lazy interpreter. It has a terrible constant overhead,
-- but, perhaps surprisingly, it passes the tower of interpreters
-- test.

module Naive where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IntSet (IntSet)
import qualified Data.IntSet as I
import Data.STRef

import HOAS

data Exp a = Var Int
           | Lam Int (Exp a)
           | Exp a `App` Exp a
           | Prim a
           deriving Show

type Env s = ReaderT (STRef s Int) (ST s)

newtype Naive s a = Naive { unNaive :: Env s (Exp a) }

fresh :: Env s Int
fresh = do
  ref <- ask
  lift . unsafeInterleaveST $ do
    x <- readSTRef ref
    writeSTRef ref $ succ x
    return x

instance Term (Naive s a) where
  Naive left % Naive right = Naive $ liftM2 App left right
  fun f = Naive $ do
    x <- fresh
    Lam x `liftM` (unNaive . f . Naive . return $ Var x)

instance PrimTerm a (Naive s a) where
  prim = Naive . return . Prim

freeVars :: Exp a -> IntSet
freeVars (Var v) = I.singleton v
freeVars (Lam v e) = I.delete v $ freeVars e
freeVars (App f a) = freeVars f `I.union` freeVars a
freeVars _ = I.empty

subst :: Int -> Exp a -> Exp a -> Env s (Exp a)
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

reduce :: Primitive a => Exp a -> Env s (Exp a)
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

eval :: Primitive a => (forall s. Naive s a) -> a
eval m = case e of
  Prim a -> a
  _ -> error "Not a prim!"
  where e = runST $ do
          ref <- newSTRef 0
          runReaderT (reduce =<< unNaive m) ref
