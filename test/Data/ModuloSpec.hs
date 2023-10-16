module Data.ModuloSpec (main, spec) where

import Data.Modulo
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "modulo arithmetic" $ do
    it "calculates gcd correctly" $
      property $
        \a m -> (\(g, _, _) -> abs g) (euclidean a m) === gcd a m

    it "implements extended euclidean algo correctly" $
      property $
        \a m -> (\(_, x, y) -> abs $ x * a + y * m) (euclidean a m) === gcd a m

    it "calculates inverse correctly" $
      property $
        \a' m' -> do
          let a = abs a'
          let m = abs m'
          maybe discard ((`mod` m) . (* a)) (inverse a m) === (1 :: Int)
