module Data.ModuloSpec (main, spec) where

import Data.Modulo
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "modulo arithmetic" $ do
    context "on matrices" $ do
      it "det(Id) = 1" $
        det idMx `shouldBe` 1

      it "product preserves determinant" $
        property $
          \a b -> det (a #=# b) === det a * det b

      it "product has left identity" $
        property $
          \a -> idMx #=# a === a

      it "product has right identity" $
        property $
          \a -> a #=# idMx === a

      it "modular inverse passes sanity" $
        property $
          mapMx (`mod` 7) <$> inverseMx ((1, 2), (3, 4)) 7
            `shouldBe` Just ((5, 1), (5, 3))

      it "implements modular inverse correctly" $
        property $
          \a m ->
            maybe
              discard
              (mapMx (`mod` m) . (a #=#))
              (inverseMx a m)
              === idMx

      it "vec product passes sanity" $ do
        ((4, 3), (2, 7)) #=| (3, 8) `shouldBe` (36, 62)
        (3, 8) |=# ((4, 3), (2, 7)) `shouldBe` (28, 65)

      it "vec product has right identity" $
        property $
          \a -> idMx #=| a === a

      it "vec product has left identity" $
        property $
          \a -> a |=# idMx === a

      it "matrix inverses form identity for vec product" $
        property $
          \a v m ->
            maybe
              discard
              (mapVec (`mod` m) . (|=# a) . (v |=#))
              (inverseMx a m)
              === mapVec (`mod` m) v

    context "on scalars" $ do
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
  where
    idMx = ((1, 0), (0, 1))
