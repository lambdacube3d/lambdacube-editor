-- generated file, do not modify!
-- 2016-02-26T10:42:57.376331000000Z

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


data CompileResult
  = CompileError (Array TypeInfo) (Array TypeInfo)
  | Compiled String Pipeline (Array TypeInfo)



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

instance encodeJsonCompileResult :: EncodeJson CompileResult where
  encodeJson v = case v of
    CompileError arg0 arg1 -> "tag" := "CompileError" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> jsonEmptyObject
    Compiled arg0 arg1 arg2 -> "tag" := "Compiled" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> "arg2" := arg2 ~> jsonEmptyObject

instance decodeJsonCompileResult :: DecodeJson CompileResult where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "CompileError" -> CompileError <$> obj .? "arg0" <*> obj .? "arg1"
      "Compiled" -> Compiled <$> obj .? "arg0" <*> obj .? "arg1" <*> obj .? "arg2"

