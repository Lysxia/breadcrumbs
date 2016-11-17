module Data.Breadcrumbs.Caterpillar where

data Buffer a = Buffer !Int !Int [a] [a]

empty :: Int -> Buffer a
push :: a -> Buffer a -> Buffer a
bufferToList :: Buffer a -> [a]

empty n = Buffer n n [] []

push a (Buffer n 0 new _) = Buffer n (n - 1) [a] new
push a (Buffer n m new old) = Buffer n (m - 1) (a : new) old

bufferToList (Buffer _ 0 new _) = new
bufferToList (Buffer n _ new old) = take n (new ++ old)

len (Buffer _ _ new old) = length new + length old
