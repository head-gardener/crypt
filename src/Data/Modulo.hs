module Data.Modulo (inverse, euclidean) where

import Control.Monad

inverse :: Int -> Int -> Maybe Int
inverse a m =
  let (g, x, _) = euclidean a m
   in guard (m /= 0 && a `mod` m /= 0 && g == 1)
        >> Just ((x `mod` m + m) `mod` m)

euclidean :: Int -> Int -> (Int, Int, Int)
euclidean a 0 = (a, 1, 0)
euclidean a b =
  let (g, x, y) = euclidean b (a `mod` b)
   in (g, y, x - (a `div` b) * y)
