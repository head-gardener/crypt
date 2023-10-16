module Data.Modulo
  ( Matrix,
    inverse,
    euclidean,
    strToMx,
    det,
    (#=#),
    (#=|),
    (|=#),
    mapMx,
    mapVec,
    inverseMx,
  )
where

import Control.Monad

type Matrix a = ((a, a), (a, a))

type Vec a = (a, a)

strToMx :: String -> Matrix Char
strToMx (a : b : c : d : _) = ((a, b), (c, d))
strToMx _ = undefined

det :: Matrix Int -> Int
det ((a, b), (c, d)) = a * d - b * c

trp :: Matrix Int -> Matrix Int
trp ((a, b), (c, d)) = ((a, c), (b, d))

(#=|) :: Matrix Int -> Vec Int -> Vec Int
m #=| v = fst $ trp $ m #=# trp (v, (0, 0))

(|=#) :: Vec Int -> Matrix Int -> Vec Int
v |=# m = fst $ (v, (0, 0)) #=# m

(#=#) :: Matrix Int -> Matrix Int -> Matrix Int
((a11, a12), (a21, a22)) #=# ((b11, b12), (b21, b22)) =
  (,)
    ( a11 * b11 + a12 * b21,
      a11 * b12 + a12 * b22
    )
    ( a21 * b11 + a22 * b21,
      a21 * b12 + a22 * b22
    )

mapMx :: (a -> b) -> Matrix a -> Matrix b
mapMx f = mapVec $ mapVec f

mapVec :: (a -> b) -> Vec a -> Vec b
mapVec f (a, b) = (f a, f b)

inverseMx :: Matrix Int -> Int -> Maybe (Matrix Int)
inverseMx mx@((a, b), (c, d)) m = do
  d' <- inverse (det mx) m
  return $ mapMx (* d') ((d, -b), (-c, a))

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
