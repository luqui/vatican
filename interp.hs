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

program :: (Term t) => t
program = 
    let_ (fun (\f -> fun (\x -> x))) $ \zero ->
    let_ (fun (\n -> fun (\f -> fun (\x -> f % (n % f % x))))) $ \succ ->
    succ % (succ % (succ % zero))

go = 
    let_ (fun (\n -> n % Vatican.prim (VAdd 1) % Vatican.prim (VInt 0))) $ \toPrim ->
    toPrim % program
