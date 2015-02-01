{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , NegativeLiterals
  #-}

import Control.Applicative ((<$>))

import Data.Int (Int8)

import Test.SmallCheck.Series (Serial, series)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.SmallCheck ((==>))
import qualified Test.Tasty.SmallCheck as SC

import ZigZagCoding


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [zigZagTests]


zigZagTests :: TestTree
zigZagTests = testGroup "Zig zag coding"
  [ SC.testProperty "decode . encode == id"
      ( \x -> (== x) . zigZagDecode . zigZagEncode $ (x :: Int8) )

  , SC.testProperty "encode (x + signum x) == (encode x) + 2"
      ( \x ->   not (x `elem` [0, minBound, maxBound])
                ==> zigZagEncode (x + signum x)
                    == (zigZagEncode (x :: Int8)) + 2
      )

  , SC.testProperty "abs x < abs y => encode x < encode y"
      ( \x ->  x /= minBound
            ==> \y -> abs x < abs (y :: Int8)
                    ==> zigZagEncode x < zigZagEncode y
      )
  ]



instance Monad m => Serial m Int8
  where series = (\x -> fromIntegral (x :: Int)) <$> series
