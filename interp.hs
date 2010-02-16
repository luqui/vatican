import qualified Vatican
import HOAS

data Value
    = VPlus
    | VAdd Integer
    | VInt Integer
    deriving Show

instance Vatican.Primitive Value where
    apply VPlus (VInt x) = VAdd x
    apply (VAdd x) (VInt y) = VInt (x+y)
    apply x y = error $ "Type error when applying (" ++ show x ++ ") to (" ++ show y ++ ")"

liftInterp :: (Term t) => (forall u. (Term u) => u) -> t
liftInterp expr = 
    let_ (fun (\x -> x)) $ \id -> 

    let_ (fun (\f -> fun (\x -> x))) $ \zero ->
    let_ (fun (\n -> fun (\f -> fun (\x -> f % (n % f % x))))) $ \succ ->

    let_ (fun (\n -> fun (\c -> n))) $ \nil ->
    let_ (fun (\x -> fun (\xs -> fun (\n -> fun (\c -> c % x % xs))))) $ \cons ->

    let_ (fun (\l -> l % id % fun (\x -> fun (\xs -> x)))) $ \head ->
    let_ (fun (\l -> l % id % fun (\x -> fun (\xs -> xs)))) $ \tail ->

    let_ (fun (\l -> fun (\n -> head % (n % tail % l)))) $ \nth -> 
    
    let_ (fun (\f -> fun (\x -> x % x) % fun (\x -> f % (x % x)))) $ \fix ->

    let_ (fun (\body -> fun (\l -> fun (\a -> fun (\v -> l % body))))) $ \mkLam ->
    let_ (fun (\left -> fun (\right -> fun (\l -> fun (\a -> fun (\v -> a % left % right)))))) $ \mkApp ->
    let_ (fun (\num -> fun (\l -> fun (\a -> fun (\v -> v % num))))) $ \mkVar ->

    let quote (ELam body) = mkLam % quote body
        quote (EApp l r) = mkApp % quote l % quote r
        quote (EVar v)   = mkVar % (iterate (succ %) zero !! fromIntegral v) in

    let_ (fun (\eval -> fun (\env -> fun (\term -> 
        term
            % fun (\body -> fun (\x -> eval % (cons % x % env) % body))
            % fun (\left -> fun (\right -> eval % env % left % (eval % env % right)))
            % (nth % env))))) $ \preEval ->
    let_ (fix % preEval % nil) $ \eval -> 
    eval % quote (getDeBruijn expr)

program :: (Term t) => t
program = 
    let_ (fun (\f -> fun (\x -> x))) $ \zero ->
    let_ (fun (\n -> fun (\f -> fun (\x -> f % (n % f % x))))) $ \succ ->
    succ % (succ % (succ % zero))

go :: (PrimTerm Value t) => t
go = 
    let_ (fun (\n -> n % prim (VAdd 1) % prim (VInt 0))) $ \toPrim ->
    toPrim % liftInterp program
