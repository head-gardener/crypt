module Data.Cipher.Hill (hill) where

import Control.Monad.Reader
import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.Cipher.Internal (pad)
import Data.List.Split (chunksOf)
import Data.Modulo

hill :: ((Char, Char), (Char, Char)) -> Cipher
hill key = Cipher $ do
  alph <- ask
  case valid alph key of
    Nothing -> lift Nothing
    Just (mx, mx') ->
      return (encoded alph (|=# mx), encoded alph (|=# mx'))
  where
    encoded alph f s =
      concat
        <$> mapM
          ( mapM (pos alph)
              >=> toVec
              >=> (mapM (val alph) . fromVec . mapVec (`mod` size alph) . f)
          )
          (chunks alph 2 s)
      where
        toVec [a, b] = Just (a, b)
        toVec _ = Nothing

        fromVec (a, b) = [a, b]

    chunks _ _ "" = []
    chunks a k s = chunksOf k $ pad a k s

    valid alph ((a, b), (c, d)) = do
      a' <- pos alph a
      b' <- pos alph b
      c' <- pos alph c
      d' <- pos alph d
      let mx = ((a', b'), (c', d'))
      mx' <- inverseMx ((a', b'), (c', d')) (size alph)
      return (mx, mx')
