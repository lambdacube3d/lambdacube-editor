{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module IRJson where

import Data.String
import Data.Text
import Data.Aeson hiding (Value,Bool)
import Data.Aeson.Types hiding (Value,Bool)
import IR
import Text.Parsec.Pos
import IREncode

type Info = (SourcePos,SourcePos,String)

newtype MyEither = MyEither (Either (Info, [Info]) (Pipeline, [Info]))
instance ToJSON MyEither where
  toJSON (MyEither e) = case e of
    Left (a,i) -> object ["tag" .- "Left", "value" .= TypeInfo a, "infos" .= fmap TypeInfo i]
    Right (p,i) -> object ["tag" .- "Right", "pipeline" .= p, "infos" .= fmap TypeInfo i]

-- instances for type info
newtype TypeInfo = TypeInfo (SourcePos,SourcePos,String)
instance ToJSON TypeInfo where
  toJSON (TypeInfo (s,e,m)) = object ["startL" .= sourceLine s, "startC" .= sourceColumn s ,"endL" .= sourceLine e, "endC" .= sourceColumn e, "text" .= m]
