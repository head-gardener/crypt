module Data.Cipher.Trans (trans) where

import Data.Cipher.Cipher
import Data.Cipher.Internal
import Data.List (elemIndex, permutations, sort)

trans :: String -> Cipher
trans =
  cipherBuilder'
    valid
    encoder
    (\(e, d) -> ((!! e) . permutations, (!! d) . permutations))
  where
    encoder = chunkEnc f' length
      where
        f' f alph s = toPosV alph s >> f s

    valid alph s = do
      s' <- nonEmptyV s >> uniqueV s >> toPosV alph s
      let perms = permutations (sort s')
      let perms' = permutations s'
      e <- elemIndex s' perms
      d <- elemIndex (sort s') perms'
      return (e, d)
