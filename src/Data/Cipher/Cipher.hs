module Data.Cipher.Cipher
  ( Cipher (..),
    makeCipher,
    makeCipherS,
    encipher,
    decipher
  )
where

import Control.Monad.Reader
import Data.Bijection
import Data.Cipher.Alphabet

type Convert = String -> Maybe String

-- A cipher algo is defined by the way
-- it encrypts and decrypts strings
-- over a known alphabet.
-- Can be extended to gobble Show types.
newtype Cipher
  = Cipher (ReaderT Alphabet Maybe (Convert, Convert))

-- The instance is free, since functor over
-- a tuple of arrows has bijection instance
-- instance Bijection Cipher (Reader Alphabet) String String where
--   fwd (Cipher c) = fwd c
--   bwd (Cipher c) = bwd c

encipher :: String -> Cipher -> String -> Maybe String 
encipher a c s = makeCipherS a c >>= ($ s) . fst

decipher :: String -> Cipher -> String -> Maybe String
decipher a c s = makeCipherS a c >>= ($ s) . snd

makeCipherS :: String -> Cipher -> Maybe (Convert, Convert)
makeCipherS a (Cipher c) = alphabet a >>= runReaderT c

makeCipher :: Alphabet -> Cipher -> Maybe (Convert, Convert)
makeCipher a (Cipher c) = runReaderT c a

-- Encryption algos can be combined.
-- Semigroup laws obviously hold.
-- instance Semigroup Cipher where
--   (Cipher a) <> (Cipher b) = Cipher x
--     where
--       x = do
--         (e, d) <- a
--         (e', d') <- b
--         return (e . e', d . d')

-- Initial object is a cipher that doesn't
-- modify the message.
-- instance Monoid Cipher where
--   mempty = Cipher $ return (id, id)

-- Ciphers form a group:
-- 0 is cipher id id,
-- + is combination of both functions,
-- (-x) is invert x.
