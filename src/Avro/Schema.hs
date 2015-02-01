{-# LANGUAGE FlexibleInstances
           , OverloadedStrings
           , TupleSections
           , TypeSynonymInstances
  #-}

module Avro.Schema
  (
  )
where

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))

import Data.Attoparsec.ByteString.Lazy (Parser, anyWord8)
import qualified Data.Attoparsec.ByteString.Lazy as APS

import Data.Bool (bool)
import Data.Int (Int32, Int64)

import Data.Monoid (mempty, mappend)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BS.Lazy
import Data.ByteString.Builder (Builder, word8, byteString)



class Schema t
  where avroNull :: t ()
        avroBool :: t Bool
        avroInt :: t Int32
        avroLong :: t Int64
        avroString :: t ByteString


instance Schema Parser
  where avroNull = return ()
        avroBool = (/= 0) <$> anyWord8
        avroInt = fromIntegral <$> anyWord8
        avroLong = fromIntegral <$> anyWord8
        avroString = APS.take . fromIntegral =<< avroLong


newtype Encoder t = Encoder { encode :: t -> Builder }

instance Schema Encoder
  where avroNull = Encoder $ const ""
        avroBool = Encoder $ bool "\0" "\1"
        avroInt = Encoder $ word8 . fromIntegral
        avroLong = Encoder $ word8 . fromIntegral
        avroString
          = Encoder $   uncurry mappend
                      . (encode avroLong . fromIntegral . BS.length &&& byteString)
