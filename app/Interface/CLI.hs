module Interface.CLI (run) where

import Control.Exception
import Control.Monad
import Data.Bifunctor (Bifunctor (first))
import Data.Cipher
import Interface.Common
import System.Console.ParseArgs
import System.Exit

data IFaceError
  = InvalidArg String
  | MalformedAlphabet String
  | MalformedKey String
  | InvalidCipher CipherError
  deriving (Show)

data CLArgs = Encrypt | Decrypt | CipherAlg | KeyFile
  deriving (Eq, Show, Ord)

argList :: [Arg CLArgs]
argList =
  [ Arg Encrypt (Just 'e') Nothing Nothing "Encipher",
    Arg Decrypt (Just 'd') Nothing Nothing "Decipher",
    Arg
      CipherAlg
      (Just 'c')
      Nothing
      (argDataRequired "[affine|hill|shift|subs|trans|vigenere]" ArgtypeString)
      "Cipher algorithm to use"
  ]

oneLine :: (Monad m) => m () -> String -> m String
oneLine err s = do
  let ls = lines s
  when (length ls /= 1) err
  return $ head ls

safeReadFile :: String -> FilePath -> IO String
safeReadFile s p = readFile p `catch` handler
  where
    handler :: IOException -> IO String
    handler _ = return s

run :: IO ()
run = do
  args <- parseArgsIO ArgsComplete argList
  alph <- safeReadFile defaultAlph "res/alphabet.txt"
  key <- readFile "res/key.txt"
  ss <- lines <$> readFile "res/in.txt"
  either (die . show) (mapM_ putStrLn) $ do
    let enc = gotArg args Encrypt
    let dec = gotArg args Decrypt
    let ciphF = getRequiredArg args CipherAlg

    when (enc && dec) $ Left $ InvalidArg bothE
    unless (enc || dec) $ Left $ InvalidArg noneE
    a <- oneLine (Left $ MalformedKey longAlph) alph
    k <- oneLine (Left $ MalformedKey lonkKey) key
    c <- matchCipher k ciphF
    pr <- first InvalidCipher $ tryMakeCipher a c

    let f = (if enc then fst else snd) pr
    first InvalidCipher $ mapM f ss
  where
    defaultAlph = "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ " -- lol
    longAlph = "Alphabet exceeds one line"
    lonkKey = "Key exceeds one line"
    bothE = "Encipher and decipher selected - choose one"
    noneE = "Select operation - encipher or decipher"

    matchCipher k "affine" = case k of
      [a, b] -> Right $ affine (a, b)
      _ -> Left $ MalformedKey $ "Expected two-char key: " ++ k
    matchCipher k "hill" = case k of
      [a, b, c, d] -> Right $ hill ((a, b), (c, d))
      _ -> Left $ MalformedKey $ "Expected four-char key: " ++ k
    matchCipher k "shift" = case k of
      [k'] -> Right $ shift k'
      _ -> Left $ MalformedKey $ "Expected one-char key: " ++ k
    matchCipher k "subs" = Right $ subs k
    matchCipher k "trans" = Right $ trans k
    matchCipher k "vigenere" = Right $ vigenere k
    matchCipher _ s = Left $ InvalidArg $ "Unknown cipher - " ++ s
