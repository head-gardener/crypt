module Data.Cipher.Error
  ( CipherError,
    tryEncipher,
    tryDecipher,
  )
where

import Data.Cipher.Cipher
import Data.Cipher.Alphabet

data CipherError = BadAlphabet | BadKey | BadInput
  deriving (Eq, Show)

tryEncipher :: String -> Cipher -> String -> Either CipherError String
tryEncipher a c s = do
  (e, _) <- tryMakeCipher a c
  maybe (Left BadInput) Right $ e s

tryDecipher :: String -> Cipher -> String -> Either CipherError String
tryDecipher a c s = do
  (_, d) <- tryMakeCipher a c
  maybe (Left BadInput) Right $ d s

tryMakeCipher :: String -> Cipher -> Either CipherError (Convert, Convert)
tryMakeCipher a c = do
  alph <- maybe (Left BadAlphabet) Right $ alphabet a
  maybe (Left BadKey) Right $ makeCipher alph c
