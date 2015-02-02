{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , NegativeLiterals
  #-}

import Control.Applicative ((<$>))

import Data.Bool (bool)
import Data.Int (Int8)

import Test.SmallCheck.Series ((\/), Series, generate)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.SmallCheck ((==>), over)
import qualified Test.Tasty.SmallCheck as SC

import ZigZagCoding


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests" [zigZagTests]


zigZagTests :: TestTree
zigZagTests = testGroup "Zig zag coding"
  [ SC.testProperty "decode . encode == id"
      ( over allInt8s
          $ \x -> (== x) . zigZagDecode . zigZagEncode $ (x :: Int8)
      )

  , SC.testProperty "encode (x + signum x) == (encode x) + 2"
      ( over allInt8s
          $ \x ->   not (x `elem` [0, minBound, maxBound])
                    ==> zigZagEncode (x + signum x)
                        == (zigZagEncode (x :: Int8)) + 2
      )

  , SC.testProperty "abs x < abs y => encode x < encode y"
      ( over allInt8s
          $ \x ->   x /= minBound
                    ==> over allInt8s
                          $ \y ->   abs x < abs (y :: Int8)
                                    ==> zigZagEncode x < zigZagEncode y
      )
  ]


-- Generator ignores depth and just fully explores Int8
allInt8s :: Monad m => Series m Int8
allInt8s = generate $ const [minBound..maxBound]

int8s :: Monad m => Series m Int8
int8s = maybe0 \/ positive \/ negative
  where maybe0 = generate $ \d -> bool [0] [] $ d >= 0
        positive = generate $ \d -> [1 .. toInt8 d]
        negative = negate <$> positive

        toInt8 :: Int -> Int8
        toInt8 d = fromIntegral $ min d (fromIntegral (maxBound :: Int8))
