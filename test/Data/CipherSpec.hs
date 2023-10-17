module Data.CipherSpec (main, spec) where

import Control.Monad (forM_, void)
import Data.Cipher
import Data.Cipher.Internal
import Data.List (permutations)
import Data.Modulo (strToMx)
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
        property $
          \o -> o `elem` alphanumStr ==> testCipher $ shift o

    context "affine" $ do
      it "passes sanity" $
        encipher "0123" (affine ('3', '1')) "0123" `shouldBe` Just "3210"

      it "is reversible" $ do
        property $
          \b ->
            b `elem` alphanumStr ==>
              testCipher $
                affine (alphanumStr !! 2, b)

    context "subs" $ do
      it "passes sanity" $
        encipher "0123" (subs "3120") "0123" `shouldBe` Just "3120"

      it "is reversible" $ property $ do
        \n ->
          n >= 0 && n < 1585267068834414592 ==>
            testCipher $
              subs $
                permutations alphanumStr !! n

      it "validates keys" $ do
        let f want is =
              (is, void $ makeCipherS "abc" $ subs is)
                `shouldBe` (is, want)
        forM_ ["", "abcd", "eqw", "aab"] (f Nothing)
        forM_ ["cab"] (f $ Just ())

    context "hill" $ do
      it "passes sanity" $
        do
          encipher "0123456" (hill $ strToMx "1234") "0123"
          `shouldBe` Just "3442"

      it "is reversible" $ do
        property $
          testCipher' (pad alphanum 2) $
            hill $
              strToMx "1234"

      it "validates keys" $ do
        let f want is =
              (is, void $ makeCipherS "0123456" $ hill $ strToMx is)
                `shouldBe` (is, want)
        forM_ ["0000", "1237"] (f Nothing)
        forM_ ["1001", "1234"] (f $ Just ())

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

    context "cipher chains" $ do
      it "are reversible" $ do
        property $
          testCipher $
            vigenere "abcd"
              <> shift 'e'
              <> affine ('b', 'Q')
              <> subs (reverse alphanumStr)
              <> vigenere "2t8v"
              <> shift '9'
              <> subs (permutations alphanumStr !! 5231)
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
