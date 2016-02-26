-- generated file, do not modify!
-- 2016-02-26T10:42:57.376331000000Z

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module TypeInfo where

import Data.Int
import Data.Word
import Data.Map
import Data.Vector (Vector(..))
import LambdaCube.Linear

import Data.Text
import Data.Aeson hiding (Value,Bool)
import Data.Aeson.Types hiding (Value,Bool)
import Control.Monad

import LambdaCube.IR

data TypeInfo
  = TypeInfo
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  , text :: String
  }

  deriving (Show, Eq, Ord)

data CompileResult
  = CompileError (Vector TypeInfo) (Vector TypeInfo)
  | Compiled String Pipeline (Vector TypeInfo)
  deriving (Show, Eq, Ord)


instance ToJSON TypeInfo where
  toJSON v = case v of
    TypeInfo{..} -> object
      [ "tag" .= ("TypeInfo" :: Text)
      , "startLine" .= startLine
      , "startColumn" .= startColumn
      , "endLine" .= endLine
      , "endColumn" .= endColumn
      , "text" .= text
      ]

instance FromJSON TypeInfo where
  parseJSON (Object obj) = do
    tag <- obj .: "tag"
    case tag :: Text of
      "TypeInfo" -> do
        startLine <- obj .: "startLine"
        startColumn <- obj .: "startColumn"
        endLine <- obj .: "endLine"
        endColumn <- obj .: "endColumn"
        text <- obj .: "text"
        pure $ TypeInfo
          { startLine = startLine
          , startColumn = startColumn
          , endLine = endLine
          , endColumn = endColumn
          , text = text
          } 
  parseJSON _ = mzero

instance ToJSON CompileResult where
  toJSON v = case v of
    CompileError arg0 arg1 -> object [ "tag" .= ("CompileError" :: Text), "arg0" .= arg0, "arg1" .= arg1]
    Compiled arg0 arg1 arg2 -> object [ "tag" .= ("Compiled" :: Text), "arg0" .= arg0, "arg1" .= arg1, "arg2" .= arg2]

instance FromJSON CompileResult where
  parseJSON (Object obj) = do
    tag <- obj .: "tag"
    case tag :: Text of
      "CompileError" -> CompileError <$> obj .: "arg0" <*> obj .: "arg1"
      "Compiled" -> Compiled <$> obj .: "arg0" <*> obj .: "arg1" <*> obj .: "arg2"
  parseJSON _ = mzero

