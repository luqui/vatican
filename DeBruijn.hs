module DeBruijn where

import HOAS
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Control.Applicative

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
    DeBruijn t % DeBruijn u = DeBruijn $ liftA2 EApp t u
    fun f = DeBruijn $ do
        varid <- lift get
        lift $ put (succ varid)
        local (Map.insert varid 0 . Map.map succ) $ do
            fmap ELam . rundB . f . DeBruijn $ do
                EVar <$> asks (Map.! varid)

getDeBruijn :: DeBruijn -> Exp
getDeBruijn dB = evalState (runReaderT (rundB dB) Map.empty) 0
