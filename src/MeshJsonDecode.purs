module MeshJsonDecode where

import Data.Argonaut ((~>), (:=), (.?), jsonEmptyObject, printJson)
import Data.Argonaut.Encode (EncodeJson, encodeJson)
import Data.Argonaut.Decode (DecodeJson, decodeJson)

import Data.Maybe
import Data.StrMap
import IR
import Mesh

import PipelineJsonDecode

instance decodeJsonMeshPrimitive :: DecodeJson MeshPrimitive where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "P_Points"          -> pure P_Points
      "P_TriangleStrip"   -> pure P_TriangleStrip
      "P_Triangles"       -> pure P_Triangles
      "P_TriangleStripI"  -> P_TriangleStripI <$> obj .? "values"
      "P_TrianglesI"      -> P_TrianglesI <$> obj .? "values"

instance decodeJsonMeshAttribute :: DecodeJson MeshAttribute where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .? "tag"
    case tag of
      "A_Float" -> A_Flat AT_Float <$> obj .? "values"
      "A_V2F"   -> A_Flat AT_V2F   <$> obj .? "values"
      "A_V3F"   -> A_Flat AT_V3F   <$> obj .? "values"
      "A_V4F"   -> A_Flat AT_V4F   <$> obj .? "values"
      "A_M22F"  -> A_Flat AT_M22F  <$> obj .? "values"
      "A_M33F"  -> A_Flat AT_M33F  <$> obj .? "values"
      "A_M44F"  -> A_Flat AT_M44F  <$> obj .? "values"

newtype MeshData = MeshData Mesh
instance decodeJsonMesh :: DecodeJson MeshData where
  decodeJson json = do
    obj <- decodeJson json
    attributes <- obj .? "attributes"
    primitive <- obj .? "primitive"
    pure $ MeshData {attributes:attributes,primitive:primitive,gpuData:Nothing}
