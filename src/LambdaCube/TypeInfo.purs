-- generated file, do not modify!
-- 2016-11-14T21:30:03.153267000000Z

module LambdaCube.TypeInfo where
import Prelude
import Data.Generic
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.StrMap (StrMap(..))
import Data.Map (Map(..))
import Data.List (List(..))
import LambdaCube.LinearBase

import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Argonaut.Decode.Combinators ((.?))
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Printer (printJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)

import LambdaCube.IR

data Range
  = Range
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  }


data TypeInfo
  = TypeInfo
  { range :: Range
  , text :: String
  }


data WarningInfo
  = WarningInfo
  { wRange :: Range
  , wText :: String
  }


data ErrorInfo
  = ErrorInfo
  { eRange :: Range
  , eText :: String
  }


data CompileResult
  = CompileError String (Array TypeInfo) (Array WarningInfo) (Array ErrorInfo)
  | Compiled String String Pipeline (Array TypeInfo) (Array WarningInfo)



instance encodeJsonRange :: EncodeJson Range where
  encodeJson v = case v of
    Range r ->
      "tag" := "Range" ~>
      "startLine" := r.startLine ~>
      "startColumn" := r.startColumn ~>
      "endLine" := r.endLine ~>
      "endColumn" := r.endColumn ~>
      jsonEmptyObject

instance decodeJsonRange :: DecodeJson Range where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "Range" -> do
        startLine <- obj .? "startLine"
        startColumn <- obj .? "startColumn"
        endLine <- obj .? "endLine"
        endColumn <- obj .? "endColumn"
        pure $ Range
          { startLine:startLine
          , startColumn:startColumn
          , endLine:endLine
          , endColumn:endColumn
          } 
      _ -> Left ("decodeJsonRange - unknown tag: " <> tag)

instance encodeJsonTypeInfo :: EncodeJson TypeInfo where
  encodeJson v = case v of
    TypeInfo r ->
      "tag" := "TypeInfo" ~>
      "range" := r.range ~>
      "text" := r.text ~>
      jsonEmptyObject

instance decodeJsonTypeInfo :: DecodeJson TypeInfo where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "TypeInfo" -> do
        range <- obj .? "range"
        text <- obj .? "text"
        pure $ TypeInfo
          { range:range
          , text:text
          } 
      _ -> Left ("decodeJsonTypeInfo - unknown tag: " <> tag)

instance encodeJsonWarningInfo :: EncodeJson WarningInfo where
  encodeJson v = case v of
    WarningInfo r ->
      "tag" := "WarningInfo" ~>
      "wRange" := r.wRange ~>
      "wText" := r.wText ~>
      jsonEmptyObject

instance decodeJsonWarningInfo :: DecodeJson WarningInfo where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "WarningInfo" -> do
        wRange <- obj .? "wRange"
        wText <- obj .? "wText"
        pure $ WarningInfo
          { wRange:wRange
          , wText:wText
          } 
      _ -> Left ("decodeJsonWarningInfo - unknown tag: " <> tag)

instance encodeJsonErrorInfo :: EncodeJson ErrorInfo where
  encodeJson v = case v of
    ErrorInfo r ->
      "tag" := "ErrorInfo" ~>
      "eRange" := r.eRange ~>
      "eText" := r.eText ~>
      jsonEmptyObject

instance decodeJsonErrorInfo :: DecodeJson ErrorInfo where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "ErrorInfo" -> do
        eRange <- obj .? "eRange"
        eText <- obj .? "eText"
        pure $ ErrorInfo
          { eRange:eRange
          , eText:eText
          } 
      _ -> Left ("decodeJsonErrorInfo - unknown tag: " <> tag)

instance encodeJsonCompileResult :: EncodeJson CompileResult where
  encodeJson v = case v of
    CompileError arg0 arg1 arg2 arg3 -> "tag" := "CompileError" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> "arg2" := arg2 ~> "arg3" := arg3 ~> jsonEmptyObject
    Compiled arg0 arg1 arg2 arg3 arg4 -> "tag" := "Compiled" ~> "arg0" := arg0 ~> "arg1" := arg1 ~> "arg2" := arg2 ~> "arg3" := arg3 ~> "arg4" := arg4 ~> jsonEmptyObject

instance decodeJsonCompileResult :: DecodeJson CompileResult where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "CompileError" -> CompileError <$> obj .? "arg0" <*> obj .? "arg1" <*> obj .? "arg2" <*> obj .? "arg3"
      "Compiled" -> Compiled <$> obj .? "arg0" <*> obj .? "arg1" <*> obj .? "arg2" <*> obj .? "arg3" <*> obj .? "arg4"
      _ -> Left ("decodeJsonCompileResult - unknown tag: " <> tag)

