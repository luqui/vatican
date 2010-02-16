module HOAS (Term(..), let_, DeBruijn, quote) where

import qualified Vatican
import qualified Data.Map as Map
import Control.Monad (liftM2)
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

infixl 9 %
class Term t where
    (%) :: t -> t -> t
    fun :: (t -> t) -> t

instance Term (Vatican.Term a) where
    (%) = (Vatican.%)
    fun = Vatican.fun

let_ :: (Term t) => t -> (t -> t) -> t
let_ defn body = fun body % defn


newtype DeBruijn t = DeBruijn { rundB :: ReaderT (Integer -> t) (State (Map.Map Integer Integer, Integer)) t }

instance Term t => Term (DeBruijn t) where
    DeBruijn t % DeBruijn u = DeBruijn $ liftM2 (%) t u
    fun f = DeBruijn $ do
        (mp, varid) <- lift get
        lift $ put (Map.insert varid 0 (Map.map succ mp), succ varid)
        quoter <- ask
        rundB . f . DeBruijn $ do
            mp' <- lift (gets fst)
            return . quoter $ mp' Map.! varid


quote :: Term t => (Integer -> t) -> DeBruijn t -> t 
quote quoter dB = evalState (runReaderT (rundB dB) quoter) (Map.empty, 0)
