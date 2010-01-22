import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Char (isNumber)

data Var
    = VVar String
    | VCx String [String]

data Term
    = TVar Var
    | TLam String Var
    | TApp Term Var
    | TLet Var Term Term

showVar :: Var -> String
showVar (VVar v) = v
showVar (VCx z zs) = z ++ "(" ++ intercalate "," zs ++ ")"

showTerm :: Term -> String
showTerm (TVar v) = showVar v
showTerm (TApp t v) = "(" ++ showTerm t ++ " " ++ showVar v ++ ")"
showTerm (TLam v b) = "(\\" ++ v ++ ". " ++ showVar b ++ ")"
showTerm (TLet v t b) = "let " ++ showVar v ++ " = " ++ showTerm t ++ " in " ++ showTerm b

instance Show Var where
    show = showVar

instance Show Term where
    show = showTerm



data Expr
    = ELam String Expr
    | EApp Expr Expr
    | EVar String
    deriving Show

infixl 9 %
(%) = EApp
lam = ELam
var = EVar
prim = EPrim



compile :: Expr -> State Int Term
compile = go []
    where
    go zs (EVar v) = return $ TVar (VVar v)
    go zs (ELam x (EVar y)) = return $ TLam x (VVar y)  -- fresh?
    go zs (ELam x e) = do
        y <- fresh x
        z <- fresh "Z"
        e' <- go (zs ++ [x]) e
        return $ TLet (VCx z (zs ++ [x])) e' (TLam y (VCx z (zs ++ [y])))
    go zs (EApp t (EVar u)) = fmap (`TApp` VVar u) (go zs t)
    go zs (EApp t u) = do
        t' <- go zs t
        u' <- go zs u
        z <- fresh "Z"
        return $ TLet (VCx z zs) u' (TApp t' (VCx z zs))

fresh name = do
    n <- get
    put (n+1)
    let name' = reverse . dropWhile isNumber . reverse $ name
    return $ name' ++ show n


data Env = Env {
    envVarMap :: Map.Map String Term,
    envCxMap :: Map.Map String ([String], Term)
}

insertVar :: String -> Term -> Env -> Env
insertVar v t e = e { envVarMap = Map.insert v t (envVarMap e) }

insertCx :: String -> [String] -> Term -> Env -> Env
insertCx z zs t e = e { envCxMap = Map.insert z (zs,t) (envCxMap e) }

eval :: Env -> Term -> WriterT [(Env, Term)] (State Int) (Env, Term)
eval = trace go
    where
    go env (TLam x t) = return (env, TLam x t)
    go env (TApp t b) = do
        (env',t') <- eval env t
        case t' of
            TLam y b' -> do
                (env'', v) <- eval (insertVar y (TVar b) env') (TVar b')
                return (env'', v)
            x -> return (env', TApp x b)
    go env (TLet (VVar v) u t) = eval (insertVar v u env) t 
    go env (TLet (VCx z zs) u t) = eval (insertCx z zs u env) t
    go env (TVar (VCx z ys)) = do
        let (xs, t) = lookupCx z env
        (env', v) <- eval env t
        v' <- lift (freshify v)
        eval (insertCx z xs v env') (subst (zip xs ys) v')
    go env (TVar (VVar x)) = do
        case lookupVar x env of
            Just t -> do 
                (env',u) <- eval env t
                eval (insertVar x u env') u
            Nothing -> return (env, TVar (VVar x))

    trace w e t = do
        tell [(e,t)]
        (e',t') <- w e t
        tell [(e',t')]
        return (e',t')

freshify :: Term -> State Int Term
freshify (TLam x v) = do
    x' <- fresh x
    return (TLam x' (substVar [(x,x')] v))
freshify (TApp a b) = do
    a' <- freshify a
    return (TApp a' b)
freshify (TLet b u t) = do
    t' <- freshify t
    return (TLet b u t')   -- lets too?
freshify (TVar v) = return (TVar v)


subst :: [(String,String)] -> Term -> Term
subst m (TLam x t) = TLam x (substVar (filter (\a -> fst a /= x) m) t)
subst m (TApp t v) = TApp (subst m t) (substVar m v)
subst m (TLet b u t) = TLet b u (subst m t)  -- capture?
subst m (TVar v) = TVar (substVar m v)

substVar :: [(String,String)] -> Var -> Var
substVar m (VVar x) = VVar (substName m x)
substVar m (VCx z xs) = VCx z (map (substName m) xs)

substName :: [(String,String)] -> String -> String
substName m x = maybe x id (lookup x m)

lookupCx :: String -> Env -> ([String], Term)
lookupCx z e = 
    case Map.lookup z (envCxMap e) of
        Just r -> r
        Nothing -> error $ "Context '" ++ z ++ "' not in environment!"

lookupVar :: String -> Env -> Maybe Term
lookupVar v e = Map.lookup v (envVarMap e)

run :: Expr -> [(Env,Term)]
run expr = evalState go 0
    where
    go = do
        term <- compile expr
        execWriterT (eval (Env Map.empty Map.empty) term)

printE :: (Env,Term) -> String
printE (env,term) = 
       intercalate "\n" (map showC (Map.assocs (envCxMap  env))) ++ "\n"
    ++ intercalate "\n" (map showV (Map.assocs (envVarMap env))) ++ "\n"
    ++ "-----\n" 
    ++ show term ++ "\n\n"
    where
    showV (a,b) = a ++ " -> " ++ show b
    showC (z,(zs,t)) = show (VCx z zs) ++ " -> " ++ show t
