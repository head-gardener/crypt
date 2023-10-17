module Data.Cipher.Alphabet
  ( Alphabet,
    alphabet,
    pos,
    val,
    size,
    alphanum,
    alphanumStr,
    vals,
    pos',
    valsOrd,
  )
where

import Control.Monad
import Data.Bimap ((!), (!?), (!?>))
import qualified Data.Bimap as BM
import Data.Maybe (fromMaybe)
import Data.List (sortBy)
import Data.Function (on)

newtype Alphabet = Alphabet (BM.Bimap Char Int)
  deriving (Eq, Show)

alphabet :: String -> Maybe Alphabet
alphabet = validate >=> foldr f (Just BM.empty) . zip [0 ..] >=> return . Alphabet
  where
    validate s = do
      let ls = lines s
      guard $ length ls == 1
      Just $ head ls

    f _ Nothing = Nothing
    f (i, c) (Just a) =
      if BM.notMember c a
        then Just $ BM.insert c i a
        else Nothing

alphabet' :: String -> Alphabet
alphabet' = fromMaybe undefined . alphabet

pos :: Alphabet -> Char -> Maybe Int
pos (Alphabet m) = (!?) m

pos' :: Alphabet -> Char -> Int
pos' (Alphabet m) = (!) m

val :: Alphabet -> Int -> Maybe Char
val (Alphabet m) = (!?>) m

size :: Alphabet -> Int
size (Alphabet m) = BM.size m

vals :: Alphabet -> [Char]
vals (Alphabet m) = BM.keys m

valsOrd :: Alphabet -> [Char]
valsOrd (Alphabet m) = fst <$> sortBy (compare `on` snd) (BM.assocs m)

alphanum :: Alphabet
alphanum = alphabet' alphanumStr

alphanumStr :: String
alphanumStr =
  ['a' .. 'z']
    <> ['A' .. 'Z']
    <> ['0' .. '9']
    <> [' ']
