{-# LANGUAGE ConstraintKinds
           , FlexibleContexts
           , LambdaCase
           , MultiParamTypeClasses
           , NegativeLiterals
           , OverloadedStrings
           , RankNTypes
           , TypeFamilies
  #-}

import Control.Monad (replicateM)
import Control.Monad.Logic (interleave)

import Data.Int (Int8, Int32, Int64)

import Data.List (isInfixOf)

import Data.Map (Map)
import qualified Data.Map as Map

import qualified Data.Attoparsec.ByteString.Lazy as APS

import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS

import GHC.Exts (IsString, fromString)

import Test.SmallCheck.Series
  ( (><)
  , Series
  , series
  , getDepth
  , localDepth
  , generate
  , cons3
  )

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.SmallCheck ((==>), changeDepth, over, testProperty)

import ZigZagCoding (zigZagEncode, zigZagDecode)

import Data.Functor.Polyvariant
  ( (|$|)
  , (|*|)
  , (|@|)
  , Polyvariant
  , (<~>)((:/))
  )

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

      , avroRecord
      , avroEnum
      , avroArray
      , avroMap
      , avroFixed
      )
  )
import Avro.Encoder (encode)
import Avro.Parser (Parser(Parser), parse)

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Tests"
  [ zigZagTests
  , avroPrimitiveTests
  , avroCompoundTests
  , avroPolyvariantTests
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

  , testProperty "Invalid UTF-8 fails decoding"
      ( over invalidUtf8
          $ fails avroString "Invalid UTF-8"
              . toLazyByteString
              . encode avroBytes
      )
  ]


avroCompoundTests :: TestTree
avroCompoundTests = testGroup "Avro compound types"
  [ testProperty "Array: decode . encode == id" (roundTrip (avroArray avroBool))
  , testProperty "Map: decode . encode == id"
      ( over (mapsOf longerStrings series) $ roundTrip (avroMap avroBool) )
  , testProperty "Fixed: decode . encode == id"
      ( changeDepth (*20) . \n ->   over (stringsOfLength n "\NUL\255")
                  $ roundTrip (avroFixed (fromIntegral n))
      )
  ]

avroPolyvariantTests :: TestTree
avroPolyvariantTests = testGroup "Records via polyvariance"
  [ testProperty "Triple can be round-tripped"
      ( over (cons3 Triple)
          $ ( roundTripPolyvariant avroBoolNullFloat
              :: Triple Bool () Float -> Bool
            )
      )
  ]


roundTrip :: Eq a => (forall s. Schema s => s a) -> a -> Bool
roundTrip s x = parses x s . toLazyByteString . encode s $ x

roundTripPolyvariant
 :: Eq a => (forall s. (Schema s, Polyvariant s) => s a) -> a -> Bool
roundTripPolyvariant s x = parses x s . toLazyByteString . encode s $ x

fails :: Parser a -> String -> ByteString -> Bool
fails parser message
  = \case   APS.Fail _ _ e  -> message `isInfixOf` e
            APS.Done {}     -> False
    . parse parser


parses :: Eq a => a -> Parser a -> ByteString -> Bool
parses x parser
  = \case   APS.Fail {}     -> False
            APS.Done _ r    -> x == r
    . parse (parser <* Parser APS.endOfInput)


-- Generator ignores depth and just fully explores Int8
allInt8s :: Monad m => Series m Int8
allInt8s = generate $ const [minBound..maxBound]

-- NOTE: this always generates strings of length 'depth'; revisit
strings :: (IsString s, Monad m) => [Char] -> Series m s
strings cs = generate $ map fromString . flip replicateM cs

stringsOfLength :: (IsString s, Monad m) => Int -> [Char] -> Series m s
stringsOfLength n cs = generate $ \d -> take d . map fromString $ replicateM n cs

longerStrings :: (IsString s, Monad m) => Series m s
longerStrings = strings ['a', 'b']

weirderStrings :: (IsString s, Monad m) => Series m s
weirderStrings = strings ['\0', '語']

invalidUtf8 :: Monad m => Series m BS.ByteString
invalidUtf8 = generate $ flip take cs
  where cs =  [ "\x80"
              , "\xbf"
              , "A\x80"
              , "\xc0 "
              , "valid until \xc1 "
              , "\xe0  "
              , "\xc0\xaf"
              , "\xfc\x80\x80\x80\x80\xaf"
              ]


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


(.:) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
(.:) = fmap . fmap
infixr 9 .:


mapsOf :: (Monad m, Ord k) => Series m k -> Series m v -> Series m (Map k v)
mapsOf = (fmap Map.fromList . listsOf . localDepth (`div` 4)) .: (><)


listsOf :: Monad m => Series m a -> Series m [a]
listsOf = localDepth (`div` 2) . \s -> flip replicateM s =<< getDepth


data Triple a b c = Triple a b c
  deriving (Eq, Show)


unTriple :: Triple a b c -> (a, (b, (c, ())))
unTriple (Triple x y z) = (x, (y, (z, ())))

avroBoolNullFloat
 :: (Polyvariant f, Schema f)
 => f (Triple Bool () Float)
avroBoolNullFloat = Triple :/ unTriple |$| avroBool |*| avroNull |@| avroFloat
