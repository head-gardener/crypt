module Data.Cipher.Subs (subs) where

import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.Cipher.Internal
import Data.Bimap ((!?), (!?>))
import qualified Data.Bimap as BM

subs :: String -> Cipher
subs = cipherBuilder valid (const $ const mapM) (\m -> ((m !?), (m !?>)))
  where
    valid alph s = permV alph s >> foldr f (Just BM.empty) (zip s (vals alph))
      where
        f _ Nothing = Nothing
        f (i, c) (Just a) =
          if BM.notMember c a
            then Just $ BM.insert c i a
            else Nothing
