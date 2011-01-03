{-# LANGUAGE PatternGuards, TupleSections #-}

module Main where

import HOAS
import DeBruijn
import qualified BUBS
import qualified Reference
import qualified Thyer
import qualified Naive
import System.Environment (getArgs)
import qualified Data.Char as Char
import qualified Parser
import Data.List (intercalate)
import Control.Applicative

data Value
    = VSucc
    | VInt !Integer
    deriving Show

instance Primitive Value where
    apply VSucc (VInt x) = VInt (x+1)
    apply x y = error $ "Type error when applying (" ++ show x ++ ") to (" ++ show y ++ ")"
    
interpreters :: [ (String, DeBruijn.Exp Value -> IO Value) ]
interpreters = [ "bubs"  --> BUBS.eval . toHOAS
               , "thyer" --> Thyer.eval . toHOAS
               , "ref"   --> return . Reference.eval . toHOAS
               , "naive" --> return . Naive.eval . toHOAS
               ]
    where
    infix 0 -->
    (-->) = (,)

main :: IO ()
main = do
    args <- getArgs
    (interp, source) <- case args of
        [i, file] | Just interp <- lookup i interpreters -> (interp,) <$> readFile file
        [i]       | Just interp <- lookup i interpreters -> (interp,) <$> getContents
        _   -> fail $ "Usage: InterpreterStack <interp> [source], <interp> is one of " 
                   ++ intercalate "," (map fst interpreters)
    case Parser.parse source of
        Left err -> fail (show err)
        Right x -> print =<< interp (EApp (EApp x (EPrim (VInt 0))) (EPrim VSucc))
