module Data.Cipher.Error
  ( CipherError,
    tryMakeCipher,
  )
where

import Data.Cipher.Alphabet
import Data.Cipher.Cipher

data CipherError = BadAlphabet | BadKey | BadInput
  deriving (Eq, Show)

type ConvertE = String -> Either CipherError String

tryMakeCipher :: String -> Cipher -> Either CipherError (ConvertE, ConvertE)
tryMakeCipher a c = do
  alph <- maybe (Left BadAlphabet) Right $ alphabet a
  (e, d) <- maybe (Left BadKey) Right $ makeCipher alph c
  return (maybe (Left BadInput) Right . e, maybe (Left BadInput) Right . d)
