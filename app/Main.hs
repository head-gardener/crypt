module Main (main) where

import Control.Monad
import Data.Cipher
import Data.Cipher.Error (tryDecipher, tryEncipher)
import Data.Modulo (strToMx)

main :: IO ()
main = forever $ do
  s <- getLine
  either
    print
    ( \s' ->
        putStrLn ("enc: " ++ s')
          >> either print (putStrLn . ("dec: " ++)) (dec s')
    )
    $ enc s
  where
    enc = tryEncipher "1234567" $ hill $ strToMx "1234"
    dec = tryDecipher "1234567" $ hill $ strToMx "1234"
