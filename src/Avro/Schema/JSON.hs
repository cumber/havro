module Avro.Schema.JSON
  (
  )
where

import Control.Applicative (Alternative, (<|>), empty)

import Data.Aeson
  ( FromJSON(parseJSON)
  , Value(Object, Array, String, Number, Bool, Null)
  , (.:)
  )
import Data.Aeson.Types (Parser)

import Data.ByteString (ByteString)

import Data.Int (Int32, Int64)

import Data.Map (Map)

import Data.Text (Text)

import Data.Vinyl (Rec, HList)

import Avro.Records
  ( Field
  , RecordDesc
  )

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
  , SomeSchema(SomeSchema)
  )


schemaFromJSON :: Schema s => Value -> Parser (SomeSchema s)

schemaFromJSON (String s)
  = case s of
        "null"      ->  pure $ SomeSchema avroNull
        "boolean"   ->  pure $ SomeSchema avroBool
        "int"       ->  pure $ SomeSchema avroInt
        "long"      ->  pure $ SomeSchema avroLong
        "float"     ->  pure $ SomeSchema avroFloat
        "double"    ->  pure $ SomeSchema avroDouble
        "bytes"     ->  pure $ SomeSchema avroBytes
        "string"    ->  pure $ SomeSchema avroString
        _           ->  empty

schemaFromJSON (Object o)
  =     schemaFromJSON . String =<< o .: "type"
    <|> empty

schemaFromJSON _ = empty
