-- generated file, do not modify!
-- 2015-10-26T11:19:51.079271000000Z

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module TypeInfo where

import Data.Int
import Data.Word
import Data.Map
import Data.Vector (Vector(..))
import Linear

import Data.Text
import Data.Aeson hiding (Value,Bool)
import Data.Aeson.Types hiding (Value,Bool)
import Control.Monad

import IR

data TypeInfo
  = TypeInfo
  { startLine :: Int
  , startColumn :: Int
  , endLine :: Int
  , endColumn :: Int
  , text :: String
  }

  deriving (Show, Eq, Ord)

data MyEither
  = MyLeft TypeInfo (Vector TypeInfo)
  | MyRight Pipeline (Vector TypeInfo)
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

instance ToJSON MyEither where
  toJSON v = case v of
    MyLeft arg0 arg1 -> object [ "tag" .= ("MyLeft" :: Text), "arg0" .= arg0, "arg1" .= arg1]
    MyRight arg0 arg1 -> object [ "tag" .= ("MyRight" :: Text), "arg0" .= arg0, "arg1" .= arg1]

instance FromJSON MyEither where
  parseJSON (Object obj) = do
    tag <- obj .: "tag"
    case tag :: Text of
      "MyLeft" -> MyLeft <$> obj .: "arg0" <*> obj .: "arg1"
      "MyRight" -> MyRight <$> obj .: "arg0" <*> obj .: "arg1"
  parseJSON _ = mzero

