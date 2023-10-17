module Data.Cipher.Hill (hill) where

import Control.Monad.Reader
import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.Cipher.Internal
import Data.Modulo

hill :: ((Char, Char), (Char, Char)) -> Cipher
hill = cipherBuilder' valid encoder (\(mx, mx') -> ((|=# mx), (|=# mx')))
  where
    encoder = chunkEnc f $ const 2
      where
        f f' alph =
          mapM (pos alph)
            >=> toVec
            >=> f'
            >=> (mapM (val alph) . fromVec . mapVec (`mod` size alph))

        toVec [a, b] = Just (a, b)
        toVec _ = Nothing

        fromVec (a, b) = [a, b]

    valid alph ((a, b), (c, d)) = do
      [a', b', c', d'] <- toPosV alph [a, b, c, d]
      let mx = ((a', b'), (c', d'))
      mx' <- inverseMx mx (size alph)
      return (mx, mx')
