{-# OPTIONS_GHC -funbox-strict-fields #-}

-- A heap reference with transparent indirection.  I'm not convinced
-- that this does anything different than a dumb IORef.  The IORefRef
-- module implements the same interface but simply delegates to IORef.
-- Currently there does not seem to be an asymptotic difference.

module IndirRef 
    ( Ref, new, read, write, link )
where

import Prelude hiding (read)
import Data.IORef

data RefData a
    = Concrete a
    | Indirect !(Ref a)

newtype Ref a = Ref (IORef (RefData a))
    deriving (Eq)

new :: a -> IO (Ref a)
new = fmap Ref . newIORef . Concrete

squashRead :: Ref a -> IO (a, Ref a)
squashRead ref@(Ref ioref) = do
    dat <- readIORef ioref
    case dat of
        Concrete x -> return (x, ref)
        Indirect ref -> do
            (x,ref') <- squashRead ref
            writeIORef ioref (Indirect ref')
            return (x,ref')

read :: Ref a -> IO a
read = fmap fst . squashRead

write :: Ref a -> a -> IO ()
write (Ref ioref) = writeIORef ioref . Concrete

link :: Ref a -> Ref a -> IO ()
link (Ref old) = writeIORef old . Indirect
