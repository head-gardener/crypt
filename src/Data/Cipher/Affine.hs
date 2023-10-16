module Data.Cipher.Affine (affine) where

import Control.Monad.Reader
import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.Cipher.Internal
import Data.Modulo

affine :: Char -> Char -> Cipher
affine a b = Cipher $ do
  alph <- ask
  case valid alph a b of
    Nothing -> lift Nothing
    Just (a', b', i) ->
      return
        ( encoded alph $ (* a') . (+ b'),
          encoded alph $ flip (-) b' . (* i)
        )
  where
    valid alph a' b' = do
      pa <- pos alph a' -- a is in the alphabet
      pb <- pos alph b' -- b is in the alphabet
      i <- inverse pa (size alph) -- a has an inverse
      return (pa, pb, i)
