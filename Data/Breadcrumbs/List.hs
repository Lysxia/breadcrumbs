module Data.Breadcrumbs.List where

data Buffer a = Buffer !Int ![a]

empty :: Int -> Buffer a
push :: a -> Buffer a -> Buffer a
bufferToList :: Buffer a -> [a]

empty n = Buffer (n + 1) []

push a (Buffer n as) = Buffer n (a : as)

bufferToList (Buffer n as) = take n as

len :: Buffer a -> Int
len (Buffer _ n) = length n
