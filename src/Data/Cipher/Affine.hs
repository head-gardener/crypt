module Data.Cipher.Affine (affine) where

import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.Cipher.Internal
import Data.Modulo

affine :: (Char, Char) -> Cipher
affine =
  cipherBuilder'
    valid
    singleCharEnc
    (\(a, b, a') -> ((* a) . (+ b), flip (-) b . (* a')))
  where
    valid alph (a, b) = do
      [a', b'] <- toPosV alph [a, b]
      i <- inverse a' (size alph) -- a has an inverse
      return (a', b', i)
