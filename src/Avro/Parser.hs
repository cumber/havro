{-# LANGUAGE BangPatterns
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
           , GeneralizedNewtypeDeriving
           , GADTs
           , KindSignatures
           , MultiParamTypeClasses
           , OverloadedStrings
           , QuasiQuotes
           , TemplateHaskell
           , TupleSections
           , TypeFamilies
           , TypeOperators
  #-}

module Avro.Parser
  ( Parser(Parser, getAttoparsecParser)
  , parse
  )
where

import Control.Applicative (liftA2)

import qualified Data.Attoparsec.ByteString.Lazy as APS

import Data.Bits ((.&.), FiniteBits, shiftL, testBit)
import Data.List (genericReplicate)

import qualified Data.Map as Map

import Data.Binary.Get (runGet)
import Data.Binary.IEEE754 (getFloat32le, getFloat64le)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Data.Text.Encoding (decodeUtf8')

import Data.Vinyl (rtraverse)
import Data.Vinyl.Functor (Identity(Identity))

import Avro.Records (Field(fieldSchema))
import Avro.Schema
  ( Schema
      ( avroNull
      , avroBool
      , avroInt
      , avroFloat
      , avroDouble
      , avroLong
      , avroBytes
      , avroString

      , avroRecord
      , avroEnum
      , avroArray
      , avroMap
      , avroFixed
      )
  )

import ZigZagCoding (zigZagDecode)

import Data.Functor.Polyvariant
  ( Polyvariant(VarianceOf)
  , Variance(Covariance)
  )


newtype Parser a = Parser { getAttoparsecParser :: APS.Parser a }
  deriving (Functor, Applicative, Monad, Monoid)

parse :: Parser a -> LBS.ByteString -> APS.Result a
parse = APS.parse . getAttoparsecParser


instance Schema Parser
  where avroNull = return ()
        avroBool = (/= 0) <$> Parser APS.anyWord8
        avroInt = zigZagDecode . decodeVarWord <$> getVarWordBytes
        avroLong = zigZagDecode . decodeVarWord <$> getVarWordBytes
        avroFloat = runGet getFloat32le . LBS.fromStrict <$> Parser (APS.take 4)
        avroDouble = runGet getFloat64le . LBS.fromStrict <$> Parser (APS.take 8)
        avroBytes = Parser . APS.take . fromIntegral =<< avroLong
        avroString = either (fail . show) return . decodeUtf8' =<< avroBytes

        avroRecord _ = rtraverse (fmap Identity . fieldSchema)

        avroEnum = toEnum . fromIntegral <$> avroInt

        avroArray itemSchema
          = do  count <- avroLong
                let arrayBlock = flip genericCount itemSchema . abs
                if count /= 0
                  then  (++) <$> arrayBlock count <*> avroArray itemSchema
                  else  return []

        avroMap
          =     fmap Map.fromList
              . avroArray
              . uncurry (liftA2 (,))
              . (avroString,)

        avroFixed = Parser . APS.take . fromIntegral


instance Polyvariant Parser
  where type VarianceOf Parser = 'Covariance


decodeVarWord :: (FiniteBits a, Integral a) => ByteString -> a
decodeVarWord = BS.foldr' f 0
  where f b x = x `shiftL` 7 + fromIntegral (b .&. 0x7f)


getVarWordBytes :: Parser ByteString
getVarWordBytes
  = Parser $ APS.scan True
      $ \s b -> if s
                  then  Just $ testBit b 7
                  else  Nothing


genericCount :: (Monad m, Integral i) => i -> m a -> m [a]
genericCount n p = sequence (genericReplicate n p)
