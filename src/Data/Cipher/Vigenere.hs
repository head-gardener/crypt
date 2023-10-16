module Data.Cipher.Vigenere (vigenere) where

import Control.Monad.Reader
import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.List.Split (chunksOf)

vigenere :: String -> Cipher
vigenere key = Cipher $ do
  alph <- ask
  case valid alph key of
    Nothing -> lift Nothing
    Just k ->
      return
        ( encoded alph (fmap (uncurry (+)) . zip k),
          encoded alph (fmap (uncurry $ flip (-)) . zip k)
        )
  where
    encoded :: Alphabet -> ([Int] -> [Int]) -> (String -> Maybe String)
    encoded alph f s = do
      ixs <- concat <$> mapM ((f <$>) . mapM (pos alph)) (chunksOf (length key) s)
      mapM (val alph . (`mod` size alph)) ixs

    valid alph k = do
      guard $ not $ null k -- not empty
      mapM (pos alph) key -- in the alphabet
