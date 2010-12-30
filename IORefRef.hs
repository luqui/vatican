module IORefRef
    ( Ref, new, read, write, link )
where

import Prelude hiding (read)
import Data.IORef

newtype Ref a = Ref (IORef a)

new :: a -> IO (Ref a)
new = fmap Ref . newIORef

read :: Ref a -> IO a
read (Ref ioref) = readIORef ioref

write :: Ref a -> a -> IO ()
write (Ref ioref) = writeIORef ioref

link :: Ref a -> Ref a -> IO ()
link old new = write old =<< read new
