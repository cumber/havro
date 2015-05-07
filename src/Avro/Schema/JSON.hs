{-# LANGUAGE LambdaCase
           , OverloadedStrings
  #-}

module Avro.Schema.JSON
  (
  )
where

import Control.Applicative (Alternative, (<|>), empty)

import Data.Aeson
  ( FromJSON(parseJSON)
  , Value(Object, Array, String, Number, Bool, Null)
  , Object
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
  , Some(Some)
  , transform
  )


schemaFromJSON :: Schema s => Value -> Parser (Some s)

schemaFromJSON (String s)
  = case s of
        "null"      ->  p avroNull
        "boolean"   ->  p avroBool
        "int"       ->  p avroInt
        "long"      ->  p avroLong
        "float"     ->  p avroFloat
        "double"    ->  p avroDouble
        "bytes"     ->  p avroBytes
        "string"    ->  p avroString
        _           ->  empty
  where p = pure . Some

schemaFromJSON (Object o)
  =     (schemaFromJSON . String =<< o .: "type")
    <|> (schemaFromJSONObject o =<< o .: "type")

schemaFromJSON _ = empty


schemaFromJSONObject :: Schema s => Object -> Text -> Parser (Some s)

schemaFromJSONObject o "record" = parseSome $ avroRecord <$> _ <*> _

schemaFromJSONObject o "enum" = (pure . Some) avroEnum

schemaFromJSONObject o "array" = transform avroArray <$> _ --(schemaFromJSON =<< o .: "item")

schemaFromJSONObject o "map" = Some . avroMap <$> _

schemaFromJSONObject o "fixed" = Some . avroFixed <$> _

schemaFromJSONObject _ _ = empty


parseSome :: Parser (f a) -> Parser (Some f)
parseSome = fmap Some
