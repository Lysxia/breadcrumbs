module Data.Breadcrumbs.List where

data Buffer a = Buffer !Int ![a]

empty :: Int -> Buffer a
empty n = Buffer (n + 1) []

push :: a -> Buffer a -> Buffer a
push a (Buffer n as) = Buffer n (a : as)

bufferToList (Buffer n as) = take n as

len (Buffer _ n) = length n
