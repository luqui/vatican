module Parser (parse) where

import Prelude hiding (lex, exp)
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Data.Char as Char
import qualified DeBruijn as DB
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad.Trans.Reader
import Data.Traversable (sequenceA)

data Exp 
    = Lambda String Exp
    | App Exp Exp
    | Var String

type Parser = P.Parsec String ()

preds = P.satisfy . (result or . sequenceA)
    where result = (.)

lex = P.makeTokenParser P.LanguageDef {
    P.commentStart    = "{-",
    P.commentEnd      = "-}",
    P.commentLine     = "--",
    P.nestedComments  = True,
    P.identStart      = preds [ Char.isAlpha, (`elem` "_") ],
    P.identLetter     = preds [ Char.isAlphaNum, (`elem` "_-'") ],
    P.opStart         = fail "no operators",
    P.opLetter        = fail "no operators",
    P.reservedNames   = [],
    P.reservedOpNames = [ "\\", "->" ],
    P.caseSensitive   = True
}

exp :: Parser Exp
exp = foldl1 App <$> P.many1 term
    where
    term = var <|> lambda <|> parenExp
    var = Var <$> P.identifier lex
    lambda = flip (foldr Lambda)
           <$> (P.reservedOp lex "\\" *> P.many1 (P.identifier lex)) 
           <*> (P.reservedOp lex "->" *> exp)
    parenExp = P.parens lex exp

toDeBruijn :: Exp -> DB.Exp a
toDeBruijn = flip runReader Map.empty . go
    where
    go (Lambda v body) = DB.ELam <$> local (Map.insert v 0 . Map.map succ) (go body)
    go (App t u) = liftA2 DB.EApp (go t) (go u)
    go (Var v) = DB.EVar <$> asks (Map.! v)

parse :: String -> Either P.ParseError (DB.Exp a)
parse = fmap toDeBruijn . P.parse exp "<input>"
