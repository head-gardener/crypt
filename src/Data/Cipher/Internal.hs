module Data.Cipher.Internal (invert, encoded, pad) where

import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

invert :: Cipher -> Cipher
invert (Cipher r) = Cipher $ fmap swap r

encoded :: Alphabet -> (Int -> Int) -> (String -> Maybe String)
encoded alph f s = mapM (pos alph) s >>= mapM (val alph . (`mod` size alph) . f)

pad :: Alphabet -> Int -> String -> String
pad a m s =
  s
    ++ replicate
      (m - (1 + ((length s - 1) `mod` m)))
      (fromMaybe undefined $ val a 0)
