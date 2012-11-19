module Parser (parse) where

import Prelude hiding (lex, exp)
import qualified Text.Parsec as P
import qualified Text.Parsec.Token as P
import qualified Data.Char as Char
import qualified DeBruijn as DB
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Applicative
import Control.Monad.Trans.Reader
import Data.Traversable (sequenceA)
import Debug.Trace

data Exp 
    = Lambda String Exp
    | App Exp Exp
    | Var String
    deriving (Show)

type Parser = P.Parsec String ()

preds = P.satisfy . (result or . sequenceA)
    where result = (.)

opchar = P.oneOf "`~!@#$%^&*=+|;:,<.>/?"

lex = P.makeTokenParser P.LanguageDef {
    P.commentStart    = "{-",
    P.commentEnd      = "-}",
    P.commentLine     = "--",
    P.nestedComments  = True,
    P.identStart      = preds [ Char.isAlpha, (`elem` "_") ],
    P.identLetter     = preds [ Char.isAlphaNum, (`elem` "_-'") ],
    P.opStart         = opchar,
    P.opLetter        = opchar,
    P.reservedNames   = [ "quote" ],
    P.reservedOpNames = [ "\\", "->" ],
    P.caseSensitive   = True
}

opExp :: Parser Exp
opExp = go <$> exp <*> P.many (liftA2 (,) (P.operator lex) exp)
    where
    go x0 [] = x0
    go x0 ((op, x1):xs) = go (App (App (Var op) x0) x1) xs

ident :: Parser String
ident = P.identifier lex <|> P.parens lex (P.operator lex)

exp :: Parser Exp
exp = foldl1 App <$> P.many1 term
    where
    term = quotation <|> var <|> lambda <|> parenExp
    quotation = quote <$> (P.reserved lex "quote" *> term)
    var = Var <$> P.identifier lex
    lambda = flip (foldr Lambda)
           <$> (P.reservedOp lex "\\" *> P.many1 ident) 
           <*> (P.reservedOp lex "->" *> opExp)
    parenExp = P.parens lex opExp

usedVars :: Exp -> Set.Set String
usedVars (Lambda s e) = Set.insert s (Set.delete s (usedVars e))
usedVars (App f x) = usedVars f `Set.union` usedVars x
usedVars (Var x) = Set.singleton x

makeFresh :: Set.Set String -> String -> String
makeFresh set x0 = head [ x0 ++ n | n <- "":map show [0..], not (x0 `Set.member` set) ] 

quote :: Exp -> Exp
quote e = Lambda lam $ Lambda app $ Lambda inj $ go Set.empty e
    where
    used = usedVars e
    lam = makeFresh used "lam"
    app = makeFresh used "app"
    inj = makeFresh used "inj"

    go seen (Lambda v body) = App (Var lam) (Lambda v (go (Set.insert v seen) body))
    go seen (App f x)       = App (App (Var app) (go seen f)) (go seen x)
    go seen (Var v)
        | v `Set.member` seen = Var v
        | otherwise           = App (Var inj) (Var v)

toDeBruijn :: Exp -> DB.Exp a
toDeBruijn = flip runReader Map.empty . go
    where
    go (Lambda v body) = DB.ELam <$> local (Map.insert v 0 . Map.map succ) (go body)
    go (App t u) = liftA2 DB.EApp (go t) (go u)
    go (Var v) = DB.EVar <$> get v

    get v = asks (maybe (error ("Unbound variable: " ++ v)) id . Map.lookup v)
        

traced x = trace (show x) x

parse :: String -> Either P.ParseError (DB.Exp a)
parse = fmap toDeBruijn . traced . P.parse opExp "<input>"
