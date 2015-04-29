{-# LANGUAGE BangPatterns
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , FunctionalDependencies
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

module Avro.Records
  ( FieldDesc
      ( FieldDesc
      , fieldName
      , fieldDoc
      , fieldDefaultValue
      , fieldOrder
      , fieldAliases
      )
  , Order (Ascending, Descending, Ignore)
  , Field (Field, fieldDesc, fieldSchema)
  , RecordDesc
      ( RecordDesc
      , recordName
      , recordDoc
      , recordAliases
      )
  , field
  , record
  , name
  , doc
  , defaultValue
  , order
  , aliases
  , desc
  , schema
  , withDoc
  , withDefault
  , withOrder
  , withAlias
  )
where

import Control.Lens ((&), (.~), (%~))
import Control.Lens.TH (makeLensesWith, abbreviatedFields)

import Data.ByteString (ByteString)


data FieldDesc a
  = FieldDesc
      { fieldName :: ByteString
      , fieldDoc :: Maybe ByteString
      , fieldDefaultValue :: Maybe a
      , fieldOrder :: Order
      , fieldAliases :: [ByteString]
      }
  deriving  (Eq, Show)


data Order = Ascending | Descending | Ignore
  deriving  (Eq, Show)


data Field s a
  = Field
      { fieldDesc :: FieldDesc a
      , fieldSchema :: s a
      }


field :: ByteString -> FieldDesc a
field name
  = FieldDesc
      { fieldName = name
      , fieldDoc = Nothing
      , fieldDefaultValue = Nothing
      , fieldOrder = Ascending
      , fieldAliases = []
      }

data RecordDesc
  = RecordDesc
      { recordName :: ByteString
      , recordDoc :: Maybe ByteString
      , recordAliases :: [ByteString]
      }
  deriving  (Eq, Show)


record :: ByteString -> RecordDesc
record name
  = RecordDesc
      { recordName = name
      , recordDoc = Nothing
      , recordAliases = []
      }


$(makeLensesWith abbreviatedFields ''FieldDesc)
$(makeLensesWith abbreviatedFields ''Field)
$(makeLensesWith abbreviatedFields ''RecordDesc)


withDoc :: HasDoc r ByteString => r -> ByteString -> r
f `withDoc` d = f & doc .~ d

withDefault :: HasDefaultValue r (Maybe a) => r -> a -> r
f `withDefault` d = f & defaultValue .~ Just d

withOrder :: HasOrder r Order => r -> Order -> r
f `withOrder` o = f & order .~ o

withAlias :: HasAliases r [a] => r -> a -> r
f `withAlias` a = f & aliases %~ (a:)
