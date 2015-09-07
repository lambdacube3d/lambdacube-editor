{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
import Control.Monad
import Backend.GL.Mesh
import System.Environment
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Vector.Storable as SV
import qualified Data.Trie as T
import qualified Data.Map as Map

import IRJson
import IREncode

import Data.String
import Data.Text hiding (concatMap,map)
import Data.Aeson hiding (Value,Bool)
import Data.Aeson.Types hiding (Value,Bool)
import IR

instance ToJSON MeshPrimitive where
  toJSON v = case v of
    P_Points            -> object ["tag" .- "P_Points"]
    P_TriangleStrip     -> object ["tag" .- "P_TriangleStrip"]
    P_Triangles         -> object ["tag" .- "P_Triangles"]
    P_TriangleStripI a  -> object ["tag" .- "P_TriangleStripI", "values" .= SV.toList a]
    P_TrianglesI a      -> object ["tag" .- "P_TrianglesI", "values" .= SV.toList a]

class ToFlat v where
  toFlat :: v -> [Float]

instance ToFlat Float where toFlat f = [f]
instance ToFlat a => ToFlat (V2 a) where toFlat (V2 x y) = concatMap toFlat [x,y]
instance ToFlat a => ToFlat (V3 a) where toFlat (V3 x y z) = concatMap toFlat [x,y,z]
instance ToFlat a => ToFlat (V4 a) where toFlat (V4 x y z w) = concatMap toFlat [x,y,z,w]

instance ToJSON MeshAttribute where
  toJSON v = case v of
    A_Float a -> object ["tag" .- "A_Float", "values" .= (concatMap toFlat $ SV.toList a)]
    A_V2F   a -> object ["tag" .- "A_V2F", "values" .= (concatMap toFlat $ SV.toList a)]
    A_V3F   a -> object ["tag" .- "A_V3F", "values" .= (concatMap toFlat $ SV.toList a)]
    A_V4F   a -> object ["tag" .- "A_V4F", "values" .= (concatMap toFlat $ SV.toList a)]
    A_M22F  a -> object ["tag" .- "A_M22F", "values" .= (concatMap toFlat $ SV.toList a)]
    A_M33F  a -> object ["tag" .- "A_M33F", "values" .= (concatMap toFlat $ SV.toList a)]
    A_M44F  a -> object ["tag" .- "A_M44F", "values" .= (concatMap toFlat $ SV.toList a)]
    A_Int   a -> object ["tag" .- "A_Int", "values" .= SV.toList a]
    A_Word  a -> object ["tag" .- "A_Word", "values" .= SV.toList a]

toMap t = Map.fromList $ (\(n,a) -> (B.unpack n,a)) <$> T.toList t

instance ToJSON Mesh where
  toJSON Mesh{..} = object ["attributes" .= toMap mAttributes, "primitive" .= mPrimitive]

main = do
  l <- getArgs
  forM_ l $ \n -> do
    m <- loadMesh' n
    L.writeFile (n ++ ".json") $ encode m
