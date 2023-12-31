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

      it "has a noop" $
        property $
          testCipher'' id $
            shift $
              head alphanumStr

    context "affine" $ do
      it "passes sanity" $
        encipher "0123" (affine ('3', '1')) "0123" `shouldBe` Just "3210"

      it "is reversible" $
        property $
          \b ->
            b `elem` alphanumStr ==>
              testCipher $
                affine (alphanumStr !! 2, b)

      it "has a noop" $
        property $
          testCipher'' id $
            affine (alphanumStr !! 1, head alphanumStr)

    context "subs" $ do
      it "passes sanity" $
        encipher "0123" (subs "3120") "0123" `shouldBe` Just "3120"

      it "is reversible" $ property $ do
        \n ->
          let n' = limit 0 (product [1 .. length alphanumStr]) n
           in testCipher $
                subs $
                  permutations alphanumStr !! n'

      it "validates keys" $ do
        let f want is =
              (is, void $ makeCipherS "abc" $ subs is)
                `shouldBe` (is, want)
        forM_ ["", "abcd", "eqw", "aab"] (f Nothing)
        forM_ ["cab"] (f $ Just ())

      it "has a noop" $ property $ do
        testCipher'' id $ subs alphanumStr

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

      it "has a noop" $ property $ do
        testCipher'' (pad alphanum 2) $ hill $ strToMx "baab"

    context "trans" $ do
      it "passes sanity" $ do
        encipher "0123" (trans "31") "0123" `shouldBe` Just "1032"
        encipher "0123" (trans "301") "0123" `shouldBe` Just "201030"

      it "is reversible" $ do
        property $ \n i ->
          let i' = limit 4 8 i
              n' = limit 0 (product [1 .. i']) n
           in testCipher' (pad alphanum i') $
                trans $
                  permutations (take i' alphanumStr) !! n'

      it "validates keys" $ do
        let f want is =
              (is, void $ makeCipherS "abcd" $ trans is)
                `shouldBe` (is, want)
        forM_ ["", "eqw", "aab", "abcde"] (f Nothing)
        forM_ ["db", "dcab"] (f $ Just ())

      it "has a noop" $
        property $
          \i ->
            let i' = limit 1 10 i
             in testCipher'' (pad alphanum i') $ trans (take i' alphanumStr)

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

      it "has a noop" $
        property $
          \i ->
            let i' = limit 1 10 i
             in testCipher'' id $ vigenere $ replicate i' $ head alphanumStr

    context "cipher chains" $ do
      it "are reversible" $
        property $
          testCipher $
            vigenere "abcd"
              <> shift 'e'
              <> affine ('b', 'Q')
              <> subs (reverse alphanumStr)
              <> vigenere "2t8v"
              <> shift '9'
              <> subs (permutations alphanumStr !! 5231)

      it "preserve noops" $
        property $
          let a = head alphanumStr
              b = alphanumStr !! 1
           in testCipher'' (pad alphanum 4) $
                shift a
                  <> affine (b, a)
                  <> subs alphanumStr
                  <> vigenere (replicate 8 a)
                  <> hill ((b, a), (a, b))
                  <> trans (take 4 alphanumStr)
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

    testCipher'' f c s =
      if sanitized == "" && (s /= "")
        then run s === Nothing
        else run sanitized === (Just . f) sanitized
      where
        sanitized = filter (`elem` alphanumStr) s
        run str = makeCipher alphanum c >>= ($ str) . fst

    limit l u x = l + (abs x `mod` (u - l))
