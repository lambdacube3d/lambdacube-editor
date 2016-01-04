-- generated file, do not modify!
-- 2015-12-21T12:00:19.854088000000Z

module TypeInfo where
import Prelude
import Data.Generic
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap(..))
import Data.Map (Map(..))
import Data.List (List(..))
import LinearBase

import Data.Argonaut.Combinators ((~>), (:=), (.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)

import IR

data TypeInfo
  = TypeInfo
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  , text :: String
  }


data MyEither
  = MyLeft TypeInfo (Array TypeInfo)
  | MyRight Pipeline (Array TypeInfo)



instance encodeJsonTypeInfo :: EncodeJson TypeInfo where
  encodeJson v = case v of
    TypeInfo r ->
      "tag" := "TypeInfo" ~>
      "startLine" := r.startLine ~>
      "startColumn" := r.startColumn ~>
      "endLine" := r.endLine ~>
      "endColumn" := r.endColumn ~>
      "text" := r.text ~>
      jsonEmptyObject

instance decodeJsonTypeInfo :: DecodeJson TypeInfo where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TypeInfo" -> do
        startLine <- obj .? "startLine"
        startColumn <- obj .? "startColumn"
        endLine <- obj .? "endLine"
        endColumn <- obj .? "endColumn"
        text <- obj .? "text"
        pure $ TypeInfo
          { startLine:startLine
          , startColumn:startColumn
          , endLine:endLine
          , endColumn:endColumn
          , text:text
          } 

instance encodeJsonMyEither :: EncodeJson MyEither where
  encodeJson v = case v of
    MyLeft arg0 arg1 -> "tag" := "MyLeft" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject
    MyRight arg0 arg1 -> "tag" := "MyRight" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject

instance decodeJsonMyEither :: DecodeJson MyEither where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "MyLeft" -> MyLeft <$> obj .? "arg0" <*> obj .? "arg1"
      "MyRight" -> MyRight <$> obj .? "arg0" <*> obj .? "arg1"

