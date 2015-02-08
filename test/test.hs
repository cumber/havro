{-# LANGUAGE FlexibleInstances
           , LambdaCase
           , MultiParamTypeClasses
           , NegativeLiterals
           , RankNTypes
  #-}

import Control.Monad (replicateM)
import Control.Monad.Logic (interleave)

import Data.Int (Int8, Int32, Int64)

import Data.Attoparsec.ByteString.Lazy (Parser, Result(Done, Fail), parse)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (ByteString)

import GHC.Exts (IsString, fromString)

import Test.SmallCheck.Series (Series, generate)

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.SmallCheck ((==>), changeDepth, over, testProperty)

import ZigZagCoding (zigZagEncode, zigZagDecode)

import Avro.Schema
  ( Schema
      ( avroNull
      , avroBool
      , avroInt
      , avroLong
      , avroFloat
      , avroDouble
      , avroBytes
      , avroString
      )
  , encode
  )


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
          $ \x ->   x `notElem` [0, minBound, maxBound]
                    ==> zigZagEncode (x + signum x)
                        == zigZagEncode (x :: Int8) + 2
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
      ( changeDepth (*200) . over largeAndSmall
          $ roundTrip avroInt . (fromIntegral :: Int -> Int32)
      )

  , testProperty "Long: decode . encode == id"
      ( changeDepth (*200) . over largeAndSmall
          $ roundTrip avroLong . (fromIntegral :: Int -> Int64)
      )

  , testProperty "Float: decode . encode == id" (roundTrip avroFloat)

  , testProperty "Double: decode . encode == id" (roundTrip avroDouble)

  , testProperty "Bytes: decode . encode == id"
      ( over longerStrings $ roundTrip avroBytes )

  , testProperty "String: decode . encode == id"
      ( over weirderStrings $ roundTrip avroString )
  ]


roundTrip :: Eq a => (forall s. Schema s => s a) -> a -> Bool
roundTrip s x = parses x s . toLazyByteString . encode s $ x


parses :: Eq a => a -> Parser a -> ByteString -> Bool
parses x parser
  = \case   Fail {} -> False
            Done _ r -> x == r
    . parse parser


-- Generator ignores depth and just fully explores Int8
allInt8s :: Monad m => Series m Int8
allInt8s = generate $ const [minBound..maxBound]


strings :: (IsString s, Monad m) => [Char] -> Series m s
strings cs = generate $ map fromString . flip replicateM cs

longerStrings :: (IsString s, Monad m) => Series m s
longerStrings = strings ['a', 'b']

weirderStrings :: (IsString s, Monad m) => Series m s
weirderStrings = strings ['\0', '語']


largeAndSmall :: (Bounded a, Integral a, Monad m) => Series m a
largeAndSmall
  = generate $ flip take xs
  where l = minBound `div` 2
        h = maxBound `div` 2
        mn = minBound
        mx = maxBound
        xs = interleave
                (interleave [mn, mn + 1 .. l] [mx, mx - 1 .. h])
                (interleave [0, -1 .. l + 1] [1 .. h - 1])
