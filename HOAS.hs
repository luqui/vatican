{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- A simple typeclass for terms in HOAS.

module HOAS (Primitive(..), Term(..), PrimTerm(..)) where

class (Show a) => Primitive a where
    apply :: a -> a -> a

infixl 9 %
class Term t where
    (%) :: t -> t -> t
    fun :: (t -> t) -> t

    let_ :: t -> (t -> t) -> t
    let_ defn body = fun body % defn

    fix :: t
    fix = fun (\f -> fun (\x -> x % x) % fun (\x -> f % (x % x)))
    
    letrec :: ([t] -> ([t], t)) -> t
    letrec defns = scottProj dsd 2 1
        where
        dsd = fix % fun (\dsd -> 
            let_ (scottProj dsd 2 0) $ \ds ->
            let (rs,r) = defns (map (scottProj ds n) [0..n-1])
                n      = length rs
            in scottTuple [scottTuple rs, r])
        scottProj xs n i = xs % nestedFun n (!! i)
        scottTuple xs = fun (\f -> foldl (%) f xs)
    
nestedFun :: (Term t) => Int -> ([t] -> t) -> t
nestedFun 0 f = f []
nestedFun n f = fun (\x -> nestedFun (n-1) (f . (x:)))

class (Term t) => PrimTerm a t | t -> a where
    prim :: a -> t
