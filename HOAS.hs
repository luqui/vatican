{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- A simple typeclass for terms in HOAS.
-- Also some scott encodings.

module HOAS 
    ( Primitive(..)
    , Term(..)
    , PrimTerm(..)
    , scottTuple
    , scottUntuple
    , scottProj
    , scottCoprod
    , scottUncoprod
    ) where

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
    letrec defns = scottProj 2 1 % dsd
        where
        dsd = fix % fun (\dsd -> 
            let_ (scottProj 2 0 % dsd) $ \ds ->
            let (rs,r) = defns (map (\i -> scottProj n i % ds) [0..n-1])
                n      = length rs
            in listToScottTuple [listToScottTuple rs, r])


-- scottTuple 4 = \a b c d -> \elim -> elim a b c d
scottTuple :: (Term t) => Int -> t
scottTuple n = nestedFun n (\xs -> fun (\elim -> nestedApp elim xs))

-- scottUntuple 4 p = \t -> t (\a b c d -> p [a,b,c,d])
scottUntuple :: (Term t) => Int -> ([t] -> t) -> t
scottUntuple n f = fun (\t -> t % nestedFun n f)

-- scottProj 4 1 = \f -> f (\a b c d -> b)
scottProj :: (Term t) => Int -> Int -> t
scottProj n i = scottUntuple n (!! i)

listToScottTuple :: (Term t) => [t] -> t
listToScottTuple xs = nestedApp (scottTuple (length xs)) xs

-- scottCoprod 4 1 = \x -> \a b c d -> b x
scottCoprod :: (Term t) => Int -> Int -> t
scottCoprod n i = fun (\x -> nestedFun n (\ps -> (ps !! i) % x))

-- scottUncoprod [f,g,h,i] = \p -> p f g h i
scottUncoprod :: (Term t) => [t -> t] -> t
scottUncoprod fs = fun (\p -> nestedApp p (map fun fs))


nestedApp :: (Term t) => t -> [t] -> t
nestedApp = foldl (%)

nestedFun :: (Term t) => Int -> ([t] -> t) -> t
nestedFun 0 f = f []
nestedFun n f = fun (\x -> nestedFun (n-1) (f . (x:)))

class (Term t) => PrimTerm a t | t -> a where
    prim :: a -> t
