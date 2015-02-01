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

import Data.Attoparsec.ByteString.Lazy (Parser, anyWord8)

import Data.Bool (bool)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS


class Schema t
  where avroNull :: t ()
        avroBool :: t Bool


instance Schema Parser
  where avroNull = return ()

        avroBool = (/= 0) <$> anyWord8


newtype Encoder t = Encoder { encode :: t -> ByteString }

instance Schema Encoder
  where avroNull = Encoder (const "")

        avroBool = Encoder (bool "\NUL" "\1")
