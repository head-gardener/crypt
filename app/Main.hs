module Main (main) where

import Data.Cipher
import Debug.Trace

main :: IO ()
main = do
  case ciph of
    Just c -> do
      let s = fst c "0123abcdABCD"
      print s 
      print $ s >>= snd c
    Nothing -> putStrLn "invalid key"
  where
    ciph = makeCipher alphanum (trans "ewrb")
