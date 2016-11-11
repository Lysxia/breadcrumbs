module Data.Breadcrumbs.Internal where

import Control.Monad
import Data.IORef
import System.IO.Unsafe
import System.Mem.Weak

counter :: IORef Int
counter = unsafePerformIO (newIORef 0)

(<&!>) :: Monad f => f a -> (a -> b) -> f b
(<&!>) = flip (<$!>)

data WeakList a = WeakNil | WeakCons a !(Weak (WeakList a))

-- | A persistent fixed size buffer.
--
-- Elements can be appended like in a list, but older ones
-- (at depths greater than 'depthMax') may be garbage collected.
data Buffer a = Buffer
  { contents :: !(Weak (WeakList a))
    -- ^ A 'Buffer' keeps its 'contents' alive via 'newBlob'.
  , depthMod :: !Int
  , depthMax :: !Int
  , oldBlob :: !(WeakList a)
    -- ^ The 'oldBlob' keeps a span of 'depthMax' elements alive.
  , newBlob :: !(WeakList a)
    -- ^ The 'newBlob' keeps a growing span of elements alive.
  }

-- | A 'Buffer' guaranteeing that the last @n :: Int@ elements
-- before any alive element are also alive.
empty :: Int -> IO (Buffer a)
empty n =
  mkWeak WeakNil WeakNil Nothing <&!> \contents' ->
    Buffer
      { contents = contents'
      , depthMod = n
      , depthMax = n
      , oldBlob = WeakNil
      , newBlob = WeakNil
      }

-- | Append a value to a 'Buffer'.
push :: a -> Buffer a -> IO (Buffer a)
push a b =
  mkWeak newBlob' blob Nothing <&!> \contents' ->
    Buffer
      { contents = contents'
      , depthMod = depthMod'
      , depthMax = depthMax b
      , oldBlob = oldBlob'
      , newBlob = newBlob'
      }
  where
    blob = WeakCons a (contents b)
    (oldBlob', newBlob', depthMod') =
      case depthMod b + 1 >= depthMax b of
        True -> (newBlob b, blob, 1)
        False -> (oldBlob b, newBlob b, depthMod b + 1)

newtype IOStream a = IOStream { step :: IO (Maybe (a, IOStream a)) }

foldStream :: b -> (a -> IOStream a -> IO b) -> IOStream a -> IO b
foldStream b f (IOStream s) = do
  a' <- s
  case a' of
    Just (a, s') -> f a s'
    Nothing -> return b

collectStream :: (IOStream a -> IO [a]) -> IOStream a -> IO [a]
collectStream f = foldStream [] (\a s' -> (a :) <$> f s')

runStream :: IOStream a -> IO [a]
runStream = collectStream runStream

runBoundedStream :: Int -> IOStream a -> IO [a]
runBoundedStream 0 = const (return [])
runBoundedStream n =
  collectStream (runBoundedStream (n - 1))

lazyStream :: IOStream a -> [a]
lazyStream = unsafePerformIO . collectStream (return . lazyStream)

streamBuffer :: Buffer a -> IOStream a
streamBuffer b = streamBuffer' b (contents b)

-- | The buffer is required to keep the buffered elements alive.
streamBuffer' :: Buffer a -> Weak (WeakList a) -> IOStream a
streamBuffer' b c = IOStream $ do
  blob <- deRefWeak c
  case blob of
    Just (WeakCons a c') -> return (Just (a, streamBuffer' b c'))
    _ -> return Nothing

-- | Get at most a fixed number of elements.
buffer :: Int -> Buffer a -> IO [a]
buffer n = runBoundedStream n . streamBuffer

-- | Get the default number of elements
-- (set at initialization by 'empty').
bufferDef b = buffer (depthMax b + 1) b

-- | Get as many elements as possible in the buffer.
bufferMax :: Buffer a -> IO [a]
bufferMax = runStream . streamBuffer

-- | Get elements lazily.
bufferLazy :: Buffer a -> [a]
bufferLazy = lazyStream . streamBuffer
