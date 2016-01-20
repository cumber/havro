{-# LANGUAGE GADTs
           , LambdaCase
           , OverloadedStrings
           , PartialTypeSignatures
           , PolyKinds
           , ScopedTypeVariables
  #-}
{-# OPTIONS -fdefer-type-errors #-}

module Avro.Schema.JSON
  ( schemaFromJSON
  , Some(Some)
  )
where

import Control.Applicative ((<|>), empty)

import Data.Aeson
  ( FromJSON(parseJSON)
  , Value(Object, String)
  , Object
  , (.:)
  )
import Data.Aeson.Types (Parser)

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Data.Vinyl (Rec (RNil, (:&)))

import Avro.Records
  ( Field (Field)
  , RecordDesc
  , field
  , record
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

      , avroEnum
      , avroArray
      , avroMap
      , avroFixed
      )
  , (|--)
  )


data Some :: (k -> *) -> *
  where Some :: f a -> Some f

instance Schema s => FromJSON (Some s)
  where parseJSON = schemaFromJSON


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


type SomeFields s = Some (Rec (Field s))

schemaFromJSONObject :: forall s. Schema s => Object -> Text -> Parser (Some s)

schemaFromJSONObject o "record" = someRecord <$> desc <*> fields
  where someRecord :: Schema s => RecordDesc -> SomeFields s -> Some s
        someRecord d (Some fs) = Some $ d |-- fs

        desc :: Parser RecordDesc
        desc = record . encodeUtf8 <$> o .: "name" -- FIXME

        fields :: Schema s => Parser (SomeFields s)
        fields = go (Some RNil) =<< o .: "fields"
          where go :: SomeFields s -> [Value] -> Parser (SomeFields s)
                go r [] = pure r
                go (Some r) (f : fs)
                  = do  someField <- fieldFromJSON f
                        case someField of
                          Some field' -> go (Some (field' :& r)) fs

schemaFromJSONObject _ "enum" = (pure . Some) (avroEnum :: s Bool) -- FIXME

schemaFromJSONObject o "array" = someArray <$> (schemaFromJSON =<< o .: "items")
  where someArray :: Schema s => Some s -> Some s
        someArray (Some s) = Some (avroArray s)

schemaFromJSONObject o "map" = someMap <$> (schemaFromJSON =<< o .: "values")
  where someMap :: Schema s => Some s -> Some s
        someMap (Some s) = Some (avroMap s)

schemaFromJSONObject o "fixed" = Some . avroFixed <$> (o .: "size")

schemaFromJSONObject _ _ = empty


fieldFromJSON :: Schema s => Value -> Parser (Some (Field s))

fieldFromJSON (Object o)
  = do  someType <- schemaFromJSON =<< o .: "type"
        case someType of
          Some t
            -> do  desc <- field . encodeUtf8 <$> o .: "name"   -- FIXME
                   return $ Some (Field desc t)

fieldFromJSON _ = empty
