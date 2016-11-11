module Data.Breadcrumbs.DropList where

data Buffer a = Buffer !Int ![a]

empty :: Int -> Buffer a
push :: a -> Buffer a -> Buffer a
bufferToList :: Buffer a -> [a]

empty n = Buffer (n - 1) []

push a (Buffer n as) = go as' (Buffer n (a : as'))
  where
    as' = take n as
    go (_ : xs) y = go xs y
    go [] y = y

bufferToList (Buffer _ as) = as

len :: Buffer a -> Int
len (Buffer _ as) = length as
