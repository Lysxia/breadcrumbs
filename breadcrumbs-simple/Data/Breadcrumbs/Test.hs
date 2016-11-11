module Data.Breadcrumbs.Test where

appendStuff
  :: Int -> Int
  -> (Int -> as) -> (Int -> as -> as) -> (as -> int) -> int
appendStuff size iterate empty push len =
  loop iterate (empty size)
  where
    loop 0 x = len x
    loop n x = x `seq` loop (n - 1) (push n x)

