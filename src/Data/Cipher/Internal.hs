module Data.Cipher.Internal
  ( invert,
    pad,
    cipherBuilder,
    cipherBuilder',
    singleCharEnc,
    nonEmptyV,
    uniqueV,
    toPosV,
    chunks,
    permV,
    chunkEnc,
  )
where

import Control.Monad.Reader
import Data.Bifunctor (Bifunctor (bimap))
import Data.Cipher.Alphabet
import Data.Cipher.Cipher
import Data.List (nub, sort)
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)

type Processor a = (a -> Maybe a, a -> Maybe a)

type SafeProcessor a = (a -> a, a -> a)

type Encoder p a = p -> Alphabet -> (a -> Maybe a) -> Convert

type Validator a b = Alphabet -> a -> Maybe b

type Validator' a b = a -> Maybe b

nonEmptyV :: (Foldable t) => Validator' (t a) ()
nonEmptyV s = guard $ not $ null s

uniqueV :: (Eq a) => Validator' [a] ()
uniqueV s = guard $ length s == length (nub s)

permV :: Validator [Char] ()
permV alph s = guard $ sort s == sort (vals alph)

toPosV :: (Traversable t) => Validator (t Char) (t Int)
toPosV alph = mapM (pos alph)

invert :: Cipher -> Cipher
invert (Cipher r) = Cipher $ fmap swap r

singleCharEnc :: Encoder k Int
singleCharEnc _ alph f = mapM $ pos alph >=> f >=> val alph . (`mod` size alph)

chunkEnc :: ((i -> Maybe i) -> Alphabet -> (String -> Maybe String)) -> (a -> Int) -> Encoder a i
chunkEnc f' l key alph f = fmap concat . mapM (f' f alph) . chunks alph (l key)

pad :: Alphabet -> Int -> String -> String
pad a m s =
  s
    ++ replicate
      (m - (1 + ((length s - 1) `mod` m)))
      (fromMaybe undefined $ val a 0)

chunks :: Alphabet -> Int -> String -> [String]
chunks _ _ "" = []
chunks a k s = chunksOf k $ pad a k s

fromSafe :: SafeProcessor a -> Processor a
fromSafe (e, d) = (return . e, return . d)

cipherBuilder' :: Validator key key' -> Encoder key i -> (key' -> SafeProcessor i) -> (key -> Cipher)
cipherBuilder' v e p = cipherBuilder v e (fromSafe . p)

cipherBuilder :: Validator key key' -> Encoder key i -> (key' -> Processor i) -> (key -> Cipher)
cipherBuilder v e p key = Cipher $ do
  alph <- ask
  maybe
    (lift Nothing)
    (return . bimap (e key alph) (e key alph) . p)
    $ v alph key
