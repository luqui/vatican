module HOAS (Term(..), PrimTerm(..), let_, DeBruijn, getDeBruijn, Exp(..), Reference, runReference) where

import qualified Vatican
import qualified Data.Map as Map
import Control.Monad (liftM2)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Applicative

infixl 9 %
class Term t where
    (%) :: t -> t -> t
    fun :: (t -> t) -> t

instance Term (Vatican.Term a) where
    (%) = (Vatican.%)
    fun = Vatican.fun

let_ :: (Term t) => t -> (t -> t) -> t
let_ defn body = fun body % defn


class (Term t) => PrimTerm a t | t -> a where
    prim :: a -> t

instance PrimTerm a (Vatican.Term a) where
    prim = Vatican.prim


data Exp
    = ELam Exp
    | EApp Exp Exp
    | EVar Integer

showExp lp ap (ELam e) = parens lp $ "\\. " ++ showExp False False e
showExp lp ap (EApp t u) = parens ap $ showExp True False t ++ " " ++ showExp True True u
showExp lp ap (EVar z) = show z

parens False x = x
parens True x = "(" ++ x ++ ")"

instance Show Exp where
    show = showExp False False

newtype DeBruijn = DeBruijn { rundB :: ReaderT (Map.Map Integer Integer) (State Integer) Exp }

instance Term DeBruijn where
    DeBruijn t % DeBruijn u = DeBruijn $ liftM2 EApp t u
    fun f = DeBruijn $ do
        varid <- lift get
        lift $ put (succ varid)
        local (Map.insert varid 0 . Map.map succ) $ do
            fmap ELam . rundB . f . DeBruijn $ do
                EVar <$> asks (Map.! varid)

getDeBruijn :: DeBruijn -> Exp
getDeBruijn dB = evalState (runReaderT (rundB dB) Map.empty) 0


data Reference a
    = RPrim a
    | RFun (Reference a -> Reference a)

instance (Vatican.Primitive a) => Term (Reference a) where
    RPrim a % RPrim b = RPrim (a `Vatican.apply` b)
    RPrim a % RFun _  = error "Type error!"
    RFun  f % b       = f b
    fun = RFun

instance (Vatican.Primitive a) => PrimTerm a (Reference a) where
    prim = RPrim

runReference :: Reference a -> a
runReference (RPrim a) = a
runReference _ = error "Not a prim!"
