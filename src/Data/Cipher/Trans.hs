module Data.Cipher.Trans (trans) where

import Control.Monad.Reader
import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.Cipher.Internal (pad)
import Data.List (elemIndex, nub, permutations, sort)
import Data.List.Split (chunksOf)
import Data.Maybe (isJust)

trans :: String -> Cipher
trans key = Cipher $ do
  alph <- ask
  case valid alph key of
    Nothing -> lift Nothing
    Just (e, d) ->
      return
        ( encoded alph $ (!! e) . permutations,
          encoded alph $ (!! d) . permutations
        )
  where
    encoded alph f = fmap concat . mapM f' . chunks alph (length key)
      where
        f' s = do
          -- this guard is here to make my teacher happy
          guard $ all (isJust . pos alph) s
          return $ f s

    chunks :: Alphabet -> Int -> String -> [String]
    chunks _ _ "" = []
    chunks a k s = chunksOf k $ pad a k s

    valid alph s = do
      guard $ not $ null s -- not empty
      guard $ length s == length (nub s) -- unique
      s' <- mapM (pos alph) s -- in the alphabet
      let perms = permutations (sort s')
      let perms' = permutations s'
      e <- elemIndex s' perms
      d <- elemIndex (sort s') perms'
      return (e, d)
