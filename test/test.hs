{-# LANGUAGE FlexibleInstances
           , LambdaCase
           , MultiParamTypeClasses
           , NegativeLiterals
           , RankNTypes
  #-}

import Control.Applicative ((<$>))

import Data.Bool (bool)
import Data.Int (Int8, Int32, Int64)

import Data.Attoparsec.ByteString.Lazy (Parser, Result(Done, Fail), parse)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (ByteString)

import GHC.Exts (fromString)

import Test.SmallCheck.Series ((\/), Series, generate)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.SmallCheck ((==>), over, testProperty)

import ZigZagCoding (zigZagEncode, zigZagDecode)

import Avro.Schema (Schema, avroNull, avroBool, avroInt, avroLong, avroString, encode)


main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests"
  [ zigZagTests
  , avroPrimitiveTests
  ]


zigZagTests :: TestTree
zigZagTests = testGroup "Zig zag coding"
  [ testProperty "decode . encode == id"
      ( over allInt8s
          $ \x -> (== x) . zigZagDecode . zigZagEncode $ (x :: Int8)
      )

  , testProperty "encode (x + signum x) == (encode x) + 2"
      ( over allInt8s
          $ \x ->   not (x `elem` [0, minBound, maxBound])
                    ==> zigZagEncode (x + signum x)
                        == (zigZagEncode (x :: Int8)) + 2
      )

  , testProperty "abs x < abs y => encode x < encode y"
      ( over allInt8s
          $ \x ->   x /= minBound
                    ==> over allInt8s
                          $ \y ->   abs x < abs (y :: Int8)
                                    ==> zigZagEncode x < zigZagEncode y
      )

  , testProperty "positives encode to even"
      ( over allInt8s $ \x -> x > 0 ==> even (zigZagEncode x) )

  , testProperty "negatives encode to odd"
      ( over allInt8s $ \x -> x < 0 ==> odd (zigZagEncode x) )
  ]


avroPrimitiveTests :: TestTree
avroPrimitiveTests = testGroup "Avro primitives"
  [ testProperty "Null: decode . encode == id" (roundTrip avroNull)

  , testProperty "Bool: decode . encode == id" (roundTrip avroBool)

  , testProperty "Int: decode . encode == id"
      ( roundTrip avroInt . (fromIntegral :: Int -> Int32) )

  , testProperty "Long: decode . encode == id"
      ( roundTrip avroLong . (fromIntegral :: Int -> Int64) )

  , testProperty "String: decode . encode == id"
      ( roundTrip avroString . fromString )
  ]


roundTrip :: Eq a => (forall s. Schema s => s a) -> a -> Bool
roundTrip s = \x -> parses x s . toLazyByteString . encode s $ x


parses :: Eq a => a -> Parser a -> ByteString -> Bool
parses x parser
  = \case   Fail {} -> False
            Done _ r -> x == r
    . parse parser


-- Generator ignores depth and just fully explores Int8
allInt8s :: Monad m => Series m Int8
allInt8s = generate $ const [minBound..maxBound]

int8s :: Monad m => Series m Int8
int8s = special \/ positive \/ negative
  where special = generate $ \d -> bool [0, minBound] [] $ d >= 0
        positive = generate $ \d -> [1 .. toInt8 d]
        negative = negate <$> positive

        toInt8 :: Int -> Int8
        toInt8 d = fromIntegral $ min d (fromIntegral (maxBound :: Int8))
