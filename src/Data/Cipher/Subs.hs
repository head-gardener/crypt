module Data.Cipher.Subs (subs) where

import Control.Monad.Reader
import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.Vector((!))
import qualified Data.Vector as V
import Data.List (sort)

subs :: String -> Cipher
subs sa = Cipher $ do
  alph <- ask
  case valid alph sa of
    Nothing -> lift Nothing
    Just m ->
      return (encoded alph (m !), encoded alph (m !))
  where
    encoded alph f = mapM $ (f <$>) . pos alph

    valid alph s = do
      guard $ sort s == sort (vals alph) -- key is a permutation of the alphabet
      V.generateM (length s) (Just . (s !!)) 
