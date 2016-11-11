module Data.Breadcrumbs where

import Data.Foldable
import Data.Function
import System.IO.Unsafe

import qualified Data.Breadcrumbs.Internal as I

-- | A persistent fixed size buffer.
--
-- It denotes a sequence of bounded length (fixed when constructing
-- the initial empty buffer with 'empty').
newtype Buffer a = Buffer (I.Buffer a)

-- | Initialize a buffer of size @n@, i.e., it will keep
-- the last @n@ inserted elements.
empty :: Int -> Buffer a
empty = Buffer . unsafePerformIO . I.empty

-- | The size of the buffer
-- (i.e., the value passed to 'empty').
--
-- Use 'Data.Foldable.length' for the actual number of elements.
maxSize :: Buffer a -> Int
maxSize (Buffer b) = I.depthMax b

-- | Append an element to the buffer, dropping older ones.
--
-- >>> toList (push 't' $ empty 3)
-- "t"
--
-- >>> toList (push 'c' . push 'a' . push 't' . push 's' $ empty 3)
-- "cat"
push :: a -> Buffer a -> Buffer a
push a (Buffer b) = Buffer . unsafePerformIO . I.push a $ b

-- | View the sequence of elements.
bufferToList :: Buffer a -> [a]
bufferToList (Buffer b) = take (I.depthMax b) (I.bufferLazy b)

-- | Initialize a buffer from a list.
--
-- This behaves as if the head were pushed last;
-- a tail of the list will be dropped if the buffer size
-- is smaller than the length of the list.
fromList :: Int -> [a] -> Buffer a
fromList n = foldr push (empty n) . take n

instance Eq a => Eq (Buffer a) where
  (==) = (==) `on` toList

instance Ord a => Ord (Buffer a) where
  compare = compare `on` toList

instance Show a => Show (Buffer a) where
  showsPrec n b@(Buffer b_) =
    showParen (n > appPrec) $
      showString "fromList " .
      showsPrec (appPrec + 1) (I.depthMax b_) .
      showString " " .
      showsPrec (appPrec + 1) (toList b)
    where appPrec = 10

instance Read a => Read (Buffer a) where
  readsPrec n =
    readParen (n > appPrec) $ \s -> do
      ("fromList", s) <- lex s
      (n, s) <- readsPrec (appPrec + 1) s
      (as, s) <- readsPrec (appPrec + 1) s
      pure (fromList n as, s)
    where appPrec = 10

instance Foldable Buffer where
  toList = bufferToList
  foldr f x = foldr f x . toList

instance Functor Buffer where
  fmap f b@(Buffer b_) =
    fromList (I.depthMax b_) . fmap f . toList $ b

instance Traversable Buffer where
  traverse f b@(Buffer b_) =
    fmap (fromList (I.depthMax b_)) . traverse f . toList $ b

