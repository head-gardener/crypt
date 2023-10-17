module Data.Cipher.Shift (shift) where

import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.Cipher.Internal

shift :: Char -> Cipher
shift = cipherBuilder' pos singleCharEnc (\o -> ((+ o), flip (-) o))
