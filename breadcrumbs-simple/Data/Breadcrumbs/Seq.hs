module Data.Breadcrumbs.Seq where

import Data.Foldable
import qualified Data.Sequence as S

data Buffer a = Buffer !Int !(S.Seq a)

empty :: Int -> Buffer a
push :: a -> Buffer a -> Buffer a
bufferToList :: Buffer a -> [a]

empty n = Buffer n S.empty

push a (Buffer 0 as) = Buffer 0 $
  case S.viewr as of
    as' S.:> _ -> a S.<| as'
    _ -> S.singleton a
push a (Buffer n as) = Buffer (n - 1) (a S.<| as)

bufferToList (Buffer _ as) = toList as

len :: Buffer a -> Int
len (Buffer _ as) = S.length as
