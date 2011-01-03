-- A compiler for terms in HOAS to deBruijn-encoded terms.

module DeBruijn (Exp(..), DeBruijn, getDeBruijn, toHOAS) where

import HOAS
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Control.Applicative

data Exp a
    = ELam (Exp a)
    | EApp (Exp a) (Exp a)
    | EVar Int
    | EPrim a

showExp lp ap (ELam e) = parens lp $ "\\. " ++ showExp False False e
showExp lp ap (EApp t u) = parens ap $ showExp True False t ++ " " ++ showExp True True u
showExp lp ap (EVar z) = show z
showExp lp ap (EPrim a) = "[" ++ show a ++ "]"

parens False x = x
parens True x = "(" ++ x ++ ")"

instance (Show a) => Show (Exp a) where
    show = showExp False False

newtype DeBruijn a = DeBruijn { rundB :: ReaderT (Map.Map Int Int) (State Int) (Exp a) }

instance Term (DeBruijn a) where
    DeBruijn t % DeBruijn u = DeBruijn $ liftA2 EApp t u
    fun f = DeBruijn $ do
        varid <- lift get
        lift $ put (succ varid)
        local (Map.insert varid 0 . Map.map succ) $ do
            fmap ELam . rundB . f . DeBruijn $ do
                EVar <$> asks (Map.! varid)

getDeBruijn :: DeBruijn a -> Exp a
getDeBruijn dB = evalState (runReaderT (rundB dB) Map.empty) 0

toHOAS :: (Term t, PrimTerm a t) => Exp a -> t
toHOAS = go []
    where
    go env (ELam body) = fun (\x -> go (x:env) body)
    go env (EApp t u)  = go env t % go env u
    go env (EVar z)    = env !! z
    go env (EPrim p)   = prim p
