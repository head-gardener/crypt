module Data.Cipher.AlphabetSpec (main, spec) where

import Test.Hspec

import Data.Cipher.Alphabet
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "alphabet" $ do
    it "can initialize" $ do
      alphabet "abc" `shouldSatisfy` isJust
      (size <$> alphabet "abc") `shouldBe` Just (length "abc")

    it "fails on repeats" $ do
      alphabet "aba" `shouldBe` Nothing
      alphabet "aqweaq" `shouldBe` Nothing

    it "contains all characters" $ do
      let a = fromMaybe undefined $ alphabet "abc"
      mapM_ (\c -> pos a c `shouldSatisfy` isJust) "abc"

    it "maps reversibly" $ do
      let a = fromMaybe undefined $ alphabet "abc"
      mapM_ (\c -> (pos a c >>= val a) `shouldBe` Just c) "abc"

    it "preserves order" $ do
      let a = fromMaybe undefined $ alphabet "abc"
      mapM_ (\(i, c) -> (c, pos a c) `shouldBe` (c, Just i)) $ zip [0..] "abc"
