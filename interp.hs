import Vatican

data Value
    = VPlus
    | VAdd Integer
    | VInt Integer
    deriving Show

instance Primitive Value where
    apply VPlus (VInt x) = VAdd x
    apply (VAdd x) (VInt y) = VInt (x+y)
    apply x y = error $ "Type error when applying (" ++ show x ++ ") to (" ++ show y ++ ")"

program = 
    let_ (fun (\f -> fun (\x -> x))) $ \zero ->
    let_ (fun (\n -> fun (\f -> fun (\x -> f % (n % f % x))))) $ \succ ->
    let_ (fun (\n -> n % prim (VAdd 1) % prim (VInt 0))) $ \toPrim ->
    toPrim % (succ % (succ % (succ % zero)))
