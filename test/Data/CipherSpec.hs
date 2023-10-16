module Data.CipherSpec (main, spec) where

import Control.Monad (forM_, void)
import Data.Cipher
import Data.Cipher.Hill (strToMx)
import Data.Cipher.Internal
import Data.List (permutations)
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ciphers" $ do
    context "shift" $ do
      it "passes sanity" $
        encipher "0123" (shift '2') "0123" `shouldBe` Just "2301"

      it "is reversible" $ do
        property $ testCipher $ shift (alphanumStr !! 34)

    context "affine" $ do
      it "passes sanity" $
        encipher "0123" (affine '3' '1') "0123" `shouldBe` Just "3210"

      it "is reversible" $ do
        property $ testCipher $ affine (alphanumStr !! 2) (alphanumStr !! 34)

    context "subs" $ do
      it "passes sanity" $
        encipher "0123" (subs "3120") "0123" `shouldBe` Just "3120"

      it "is reversible" $ do
        property $ testCipher $ subs $ reverse alphanumStr

      it "validates keys" $ do
        let f want is =
              (is, void $ makeCipherS "abc" $ subs is)
                `shouldBe` (is, want)
        forM_ ["", "abcd", "eqw", "aab"] (f Nothing)
        forM_ ["cab"] (f $ Just ())

    context "hill" $ do
      it "passes sanity" $ do
        encipher "0123" (hill $ strToMx "0123") "0123" `shouldBe` Just "1032"

      it "is reversible" $ do
        property $
          testCipher' (pad alphanum 4) $
            trans $
              permutations (take 4 alphanumStr) !! 16

      it "validates keys" $ do
        let f want is =
              (is, void $ makeCipherS "0123456" $ hill $ strToMx is)
                `shouldBe` (is, want)
        -- FIXME: MORE!
        forM_ ["0000"] (f Nothing)
        forM_ ["1001"] (f $ Just ())

    context "trans" $ do
      it "passes sanity" $ do
        encipher "0123" (trans "31") "0123" `shouldBe` Just "1032"
        encipher "0123" (trans "301") "0123" `shouldBe` Just "201030"

      it "is reversible" $ do
        property $
          testCipher' (pad alphanum 4) $
            trans $
              permutations (take 4 alphanumStr) !! 16

      it "validates keys" $ do
        let f want is =
              (is, void $ makeCipherS "abcd" $ trans is)
                `shouldBe` (is, want)
        forM_ ["", "eqw", "aab", "abcde"] (f Nothing)
        forM_ ["db", "dcab"] (f $ Just ())

    context "vigenere" $ do
      it "passes sanity" $ do
        encipher "0123" (vigenere "31") "0123" `shouldBe` Just "3210"
        encipher "0123" (vigenere "312") "0123" `shouldBe` Just "3202"

      it "is reversible" $ do
        property $ testCipher $ vigenere "abcd"

      it "validates keys" $ do
        let f want is =
              (is, void $ makeCipherS "abcd" $ vigenere is)
                `shouldBe` (is, want)
        forM_ ["", "abcde"] (f Nothing)
        forM_ ["cab", "aab", "a"] (f $ Just ())
  where
    testCipher = testCipher' id

    testCipher' f c s =
      if sanitized == "" && (s /= "")
        then run s === Nothing
        else run sanitized === (Just . f) sanitized
      where
        sanitized = filter (`elem` alphanumStr) s
        run str = do
          (e, d) <- makeCipher alphanum c
          str' <- e str
          d str'
