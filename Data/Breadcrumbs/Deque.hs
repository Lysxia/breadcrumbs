module Data.Breadcrumbs.Deque where

data Buffer a = Buffer !Int [a] [a]

empty :: Int -> Buffer a
push :: a -> Buffer a -> Buffer a
bufferToList :: Buffer a -> [a]

empty n = Buffer n [] []

push a (Buffer 0 as []) = Buffer 0 [] $! tail (reverse (a : as))
push a (Buffer 0 as (_ : as')) = Buffer 0 (a : as) as'
push a (Buffer n as as') = Buffer (n - 1) (a : as) as'

bufferToList (Buffer _ as as') = as ++ reverse as'

len :: Buffer a -> Int
len (Buffer _ as as') = length as + length as'
