-- generated file, do not modify!
-- 2016-11-10T16:02:38.361054000000Z

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

data Range
  = Range
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  }

  deriving (Show, Eq, Ord)

data TypeInfo
  = TypeInfo
  { range :: Range
  , text :: String
  }

  deriving (Show, Eq, Ord)

data WarningInfo
  = WarningInfo
  { wRange :: Range
  , wText :: String
  }

  deriving (Show, Eq, Ord)

data ErrorInfo
  = ErrorInfo
  { eRange :: Range
  , eText :: String
  }

  deriving (Show, Eq, Ord)

data CompileResult
  = CompileError String (Vector TypeInfo) (Vector WarningInfo) (Vector ErrorInfo)
  | Compiled String String Pipeline (Vector TypeInfo) (Vector WarningInfo)
  deriving (Show, Eq, Ord)


instance ToJSON Range where
  toJSON v = case v of
    Range{..} -> object
      [ "tag" .= ("Range" :: Text)
      , "startLine" .= startLine
      , "startColumn" .= startColumn
      , "endLine" .= endLine
      , "endColumn" .= endColumn
      ]

instance FromJSON Range where
  parseJSON (Object obj) = do
    tag <- obj .: "tag"
    case tag :: Text of
      "Range" -> do
        startLine <- obj .: "startLine"
        startColumn <- obj .: "startColumn"
        endLine <- obj .: "endLine"
        endColumn <- obj .: "endColumn"
        pure $ Range
          { startLine = startLine
          , startColumn = startColumn
          , endLine = endLine
          , endColumn = endColumn
          } 
  parseJSON _ = mzero

instance ToJSON TypeInfo where
  toJSON v = case v of
    TypeInfo{..} -> object
      [ "tag" .= ("TypeInfo" :: Text)
      , "range" .= range
      , "text" .= text
      ]

instance FromJSON TypeInfo where
  parseJSON (Object obj) = do
    tag <- obj .: "tag"
    case tag :: Text of
      "TypeInfo" -> do
        range <- obj .: "range"
        text <- obj .: "text"
        pure $ TypeInfo
          { range = range
          , text = text
          } 
  parseJSON _ = mzero

instance ToJSON WarningInfo where
  toJSON v = case v of
    WarningInfo{..} -> object
      [ "tag" .= ("WarningInfo" :: Text)
      , "wRange" .= wRange
      , "wText" .= wText
      ]

instance FromJSON WarningInfo where
  parseJSON (Object obj) = do
    tag <- obj .: "tag"
    case tag :: Text of
      "WarningInfo" -> do
        wRange <- obj .: "wRange"
        wText <- obj .: "wText"
        pure $ WarningInfo
          { wRange = wRange
          , wText = wText
          } 
  parseJSON _ = mzero

instance ToJSON ErrorInfo where
  toJSON v = case v of
    ErrorInfo{..} -> object
      [ "tag" .= ("ErrorInfo" :: Text)
      , "eRange" .= eRange
      , "eText" .= eText
      ]

instance FromJSON ErrorInfo where
  parseJSON (Object obj) = do
    tag <- obj .: "tag"
    case tag :: Text of
      "ErrorInfo" -> do
        eRange <- obj .: "eRange"
        eText <- obj .: "eText"
        pure $ ErrorInfo
          { eRange = eRange
          , eText = eText
          } 
  parseJSON _ = mzero

instance ToJSON CompileResult where
  toJSON v = case v of
    CompileError arg0 arg1 arg2 arg3 -> object [ "tag" .= ("CompileError" :: Text), "arg0" .= arg0, "arg1" .= arg1, "arg2" .= arg2, "arg3" .= arg3]
    Compiled arg0 arg1 arg2 arg3 arg4 -> object [ "tag" .= ("Compiled" :: Text), "arg0" .= arg0, "arg1" .= arg1, "arg2" .= arg2, "arg3" .= arg3, "arg4" .= arg4]

instance FromJSON CompileResult where
  parseJSON (Object obj) = do
    tag <- obj .: "tag"
    case tag :: Text of
      "CompileError" -> CompileError <$> obj .: "arg0" <*> obj .: "arg1" <*> obj .: "arg2" <*> obj .: "arg3"
      "Compiled" -> Compiled <$> obj .: "arg0" <*> obj .: "arg1" <*> obj .: "arg2" <*> obj .: "arg3" <*> obj .: "arg4"
  parseJSON _ = mzero

