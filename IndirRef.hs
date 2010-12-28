module IndirRef 
    ( Ref, new, read, write, link )
where

import Prelude hiding (read)
import Data.IORef

data RefData a
    = Concrete a
    | Indirect (Ref a)

newtype Ref a = Ref (IORef (RefData a))
    deriving (Eq)

new :: a -> IO (Ref a)
new = fmap Ref . newIORef . Concrete

read :: Ref a -> IO a
read (Ref ioref) = do
    dat <- readIORef ioref
    case dat of
        Concrete x -> return x
        Indirect ref -> do
            x <- read ref
            writeIORef ioref (Concrete x)
            return x

write :: Ref a -> a -> IO ()
write (Ref ioref) = writeIORef ioref . Concrete

link :: Ref a -> Ref a -> IO ()
link (Ref old) = writeIORef old . Indirect
