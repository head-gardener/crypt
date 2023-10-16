module Data.Cipher.Hill (hill, strToMx) where

import Control.Monad.Reader
import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.Cipher.Internal
 
hill :: ((Char, Char), (Char, Char)) -> Cipher
hill ((a, b), (c, d)) = Cipher $ do
  alph <- ask
  case valid alph a of
    Nothing -> lift Nothing
    Just o -> return (encoded alph (+ o), encoded alph (flip (-) o))
  where
    valid = pos -- offset is in the alphabet

strToMx :: String -> ((Char, Char), (Char, Char))
strToMx (a:b:c:d:_) = ((a, b), (c, d))
strToMx _ = undefined
