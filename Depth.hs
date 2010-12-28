{-# LANGUAGE TupleSections, FlexibleInstances, MultiParamTypeClasses #-}

module Depth 
    ( Exp(..), ExpNode, Depth, prim, getDepth )
where

import HOAS
import qualified Data.Map as Map
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Applicative
import Control.Arrow

type ExpNode a = (Int, Exp a)

data Exp a
    = Lambda (ExpNode a)
    | Apply (ExpNode a) (ExpNode a)
    | Var
    | Prim a
    deriving Show

newtype Depth a = Depth { runDepth :: ReaderT Int (State Int) (ExpNode a) }

instance Term (Depth a) where
    Depth t % Depth u = Depth $ liftA2 ap t u
        where
        ap tt@(dt,_) tu@(du,_) = (max dt du, Apply tt tu)
    fun f = Depth $ do
        varid <- lift get
        lift $ put (succ varid)
        depth <- ask
        local succ . fmap ((depth,) . Lambda) . runDepth . f . Depth . return $ (succ depth, Var)

instance PrimTerm a (Depth a) where
    prim = Depth . return . (0,) . Prim

getDepth :: Depth a -> ExpNode a
getDepth d = evalState (runReaderT (runDepth d) 0) 0
