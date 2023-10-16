module Data.Cipher.Shift (shift) where

import Control.Monad.Reader
import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.Cipher.Internal
 
shift :: Char -> Cipher
shift offset = Cipher $ do
  alph <- ask
  case valid alph offset of
    Nothing -> lift Nothing
    Just o -> return (encoded alph (+ o), encoded alph (flip (-) o))
  where
    valid = pos -- offset is in the alphabet
