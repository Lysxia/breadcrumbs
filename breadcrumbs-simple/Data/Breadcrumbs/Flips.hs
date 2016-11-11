module Data.Breadcrumbs.Flips where

data Buffer a = Buffer !Int !Int [a] [a] [a] [a]
  deriving Show

empty :: Int -> Buffer a
push :: a -> Buffer a -> Buffer a
bufferToList :: Buffer a -> [a]

empty n = Buffer n ((n + 1) `div` 2) [] [] [] []

push a (Buffer n 0 (a' : ain) [] arev []) =
  Buffer n 0 [a] ain [a'] $! tail' arev
push a (Buffer n 0 ain (a' : aver) arev aout) =
  Buffer n 0 (a : ain) aver (a' : arev) $! tail' aout
push a (Buffer n d ain aver arev aout) =
  Buffer n (d - 1) (a : ain) aver arev aout

tail' :: [a] -> [a]
tail' (_ : t) = t
tail' [] = []

bufferToList (Buffer n _ ain aver arev aout)
  = f $ ain ++ reverse arev ++ aver ++ reverse aout
  where
    f | n `mod` 2 == 0 = id
      | otherwise = take n

len :: Buffer a -> Int
len (Buffer _ _ ain aver arev aout) =
  length ain + length aver + length arev + length aout
