{-# LANGUAGE TupleSections
           , TypeSynonymInstances
  #-}

module Avro.Schema
  (
  )
where

import Data.Bool (bool)


class Schema t
  where avroNull :: t ()
        avroBool :: t Bool


newtype Decoder t = Decoder { decode :: String -> (String, t) }

instance Schema Decoder
  where avroNull = Decoder (, ())

        avroBool = Decoder f
          where f ('\NUL':cs) = (cs, False)
                f (_:cs) = (cs, True)


newtype Encoder t = Encoder { encode :: t -> String }

instance Schema Encoder
  where avroNull = Encoder (const "")

        avroBool = Encoder (bool "\NUL" "\1")
