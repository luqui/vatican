module Vatican.Repl (Term, fun, (%), int, plus, interp) where

import Control.Monad (liftM2, (<=<))
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import qualified Data.Map as Map
import Data.Word (Word8)
import qualified Data.DList as DList
import System.Process (system)
import Data.Char (chr)

type ASTM = ReaderT (Map.Map Integer Int) (State Integer)
newtype AST = AST { unAST :: ASTM Term }

-- De bruijn variables
data Term
    = TVar Int
    | TFun Term
    | TApp Term Term
    | TPrimInt32 Int
    | TPrimPlus
    deriving Show

alloc :: ASTM Integer
alloc = do
    x <- lift get
    lift (put $! x+1)
    return x

fun :: (AST -> AST) -> AST
fun f = AST $ do
    vid <- alloc
    let var :: ASTM Term 
        var = asks (TVar . (Map.! vid))
    local (Map.insert vid 0 . Map.map (+1)) . fmap TFun $ unAST (f (AST var))

infixl 9 %
(%) :: AST -> AST -> AST
AST t % AST u = AST $ liftM2 TApp t u

int :: Int -> AST
int = AST . return . TPrimInt32

plus :: AST
plus = AST $ return TPrimPlus

toTerm :: AST -> Term
toTerm (AST ast) = evalState (runReaderT ast Map.empty) 0

toBytes :: Term -> [Word8]
toBytes = DList.toList . go
    where
    go (TVar z) | z < 255 = DList.fromList [2,fromIntegral z]
                | otherwise = error "Nesting too deep (sorry)"
    go (TFun t) = 0 `DList.cons` go t
    go (TApp l r) = 1 `DList.cons` (go l `DList.append` go r)
    go (TPrimInt32 z) = 3 `DList.cons` DList.fromList (intToMSB z)
    go TPrimPlus = DList.singleton 4

interp :: AST -> IO ()
interp ast = do
    writeBytecode "bytecode.vat" ast
    system "./interp bytecode.vat"
    return ()

writeBytecode :: FilePath -> AST -> IO ()
writeBytecode file = writeFile file . map (chr . fromIntegral) . toBytes . toTerm 

intToMSB :: Int -> [Word8]
intToMSB z = map fromIntegral [z4,z3,z2,z1]
    where
    (r1,z1) = z `divMod` 256
    (r2,z2) = r1 `divMod` 256
    (r3,z3) = r2 `divMod` 256
    z4 = r3
