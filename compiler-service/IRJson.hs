{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module IRJson where

import Data.String
import Data.Text
import Data.Aeson hiding (Value,Bool)
import Data.Aeson.Types hiding (Value,Bool)
import IR
import Text.Parsec.Pos

(.-) :: Text -> Text -> Pair
a .- b = a .= b

instance ToJSON a => ToJSON (V2 a) where
  toJSON (V2 x y) = object ["x" .= x, "y" .= y]

instance ToJSON a => ToJSON (V3 a) where
  toJSON (V3 x y z) = object ["x" .= x, "y" .= y, "z" .= z]

instance ToJSON a => ToJSON (V4 a) where
  toJSON (V4 x y z w) = object ["x" .= x, "y" .= y, "z" .= z, "w" .= w]

instance ToJSON Value where
  toJSON v = case v of
    VBool  a -> object ["tag" .- "VBool"  , "value" .= a]
    VV2B   a -> object ["tag" .- "VV2B"   , "value" .= a]
    VV3B   a -> object ["tag" .- "VV3B"   , "value" .= a]
    VV4B   a -> object ["tag" .- "VV4B"   , "value" .= a]
    VWord  a -> object ["tag" .- "VWord"  , "value" .= a]
    VV2U   a -> object ["tag" .- "VV2U"   , "value" .= a]
    VV3U   a -> object ["tag" .- "VV3U"   , "value" .= a]
    VV4U   a -> object ["tag" .- "VV4U"   , "value" .= a]
    VInt   a -> object ["tag" .- "VInt"   , "value" .= a]
    VV2I   a -> object ["tag" .- "VV2I"   , "value" .= a]
    VV3I   a -> object ["tag" .- "VV3I"   , "value" .= a]
    VV4I   a -> object ["tag" .- "VV4I"   , "value" .= a]
    VFloat a -> object ["tag" .- "VFloat" , "value" .= a]
    VV2F   a -> object ["tag" .- "VV2F"   , "value" .= a]
    VV3F   a -> object ["tag" .- "VV3F"   , "value" .= a]
    VV4F   a -> object ["tag" .- "VV4F"   , "value" .= a]
    VM22F  a -> object ["tag" .- "VM22F"  , "value" .= a]
    VM23F  a -> object ["tag" .- "VM23F"  , "value" .= a]
    VM24F  a -> object ["tag" .- "VM24F"  , "value" .= a]
    VM32F  a -> object ["tag" .- "VM32F"  , "value" .= a]
    VM33F  a -> object ["tag" .- "VM33F"  , "value" .= a]
    VM34F  a -> object ["tag" .- "VM34F"  , "value" .= a]
    VM42F  a -> object ["tag" .- "VM42F"  , "value" .= a]
    VM43F  a -> object ["tag" .- "VM43F"  , "value" .= a]
    VM44F  a -> object ["tag" .- "VM44F"  , "value" .= a]

instance ToJSON InputType where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON PointSpriteCoordOrigin where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON ProvokingVertex where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON FrontFace where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON ComparisonFunction where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON StencilOperation where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON BlendEquation where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON BlendingFactor where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON LogicOperation where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON PointSize where
  toJSON v = case v of
    PointSize a -> object ["tag" .- "PointSize", "size" .= a]
    ProgramPointSize -> object ["tag" .- "ProgramPointSize"]

instance ToJSON PolygonOffset where
  toJSON v = case v of
    Offset f u -> object ["tag" .- "Offset", "factor" .= f, "units" .= u]
    NoOffset -> object ["tag" .- "NoOffset"]

instance ToJSON PolygonMode where
  toJSON v = case v of
    PolygonFill -> object ["tag" .- "PolygonFill"]
    PolygonPoint a -> object ["tag" .- "PolygonPoint", "size" .= a]
    PolygonLine a -> object ["tag" .- "PolygonLine", "size" .= a]

instance ToJSON CullMode where
  toJSON v = case v of
    CullNone -> object ["tag" .- "CullNone"]
    CullFront a -> object ["tag" .- "CullFront", "face" .= a]
    CullBack a -> object ["tag" .- "CullBack", "face" .= a]

instance ToJSON StencilOps where
  toJSON (StencilOps f b) = object ["frontStencilOp" .= f, "backStencilOp" .= b]

instance ToJSON StencilTests where
  toJSON (StencilTests f b) = object ["front" .= f, "back" .= b]

instance ToJSON StencilTest where
  toJSON (StencilTest c r m) = object ["stencilComparision" .= c, "stencilReference" .= r, "stencilMask" .= m]

instance ToJSON FetchPrimitive where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON OutputPrimitive where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON ColorArity where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON Blending where
  toJSON v = case v of
    NoBlending -> object ["tag" .- "NoBlending"]
    BlendLogicOp a -> object ["tag" .- "BlendLogicOp", "op" .= a]
    Blend (ce,ae) ((sc,dc),(sa,da)) c -> object ["tag" .- "Blend", "colorEq" .= ce, "alphaEq" .= ae
                                                , "colorF" .= object ["src" .= sc, "dst" .= dc]
                                                , "alphaF" .= object ["src" .= sa, "dst" .= da]
                                                , "color" .= c
                                                ]

instance ToJSON RasterContext where
  toJSON v = case v of
    PointCtx s t o -> object ["tag" .- "PointCtx", "size" .= s, "threshold" .= t, "origin" .= o]
    LineCtx s p -> object ["tag" .- "LineCtx", "size" .= s, "provokingVertex" .= p]
    TriangleCtx c p o v -> object ["tag" .- "TriangleCtx", "cullMode" .= c, "polygonMode" .= p, "polygonOffset" .= o, "provokingVertex" .= v]

instance ToJSON FragmentOperation where
  toJSON v = case v of
    DepthOp f m -> object ["tag" .- "DepthOp", "depthFunction" .= f, "depthMask" .= m]
    StencilOp t f b -> object ["tag" .- "StencilOp", "stencilTests" .= t, "front" .= f, "back" .= b]
    ColorOp b v -> object ["tag" .- "ColorOp", "blend" .= b, "mask" .= v]

instance ToJSON AccumulationContext where
  toJSON (AccumulationContext a b) = object ["viewportName" .= a, "operations" .= b]

instance ToJSON Image where
  toJSON v = case v of
    DepthImage l v -> object ["tag" .- "DepthImage", "layers" .= l, "value" .= v]
    StencilImage l v -> object ["tag" .- "StencilImage", "layers" .= l, "value" .= v]
    ColorImage l v -> object ["tag" .- "ColorImage", "layers" .= l, "value" .= v]

instance ToJSON TextureDataType where
  toJSON v = case v of
    FloatT a -> object ["tag" .- "FloatT", "arity" .= a]
    IntT a -> object ["tag" .- "IntT", "arity" .= a]
    WordT a -> object ["tag" .- "WordT", "arity" .= a]
    ShadowT -> object ["tag" .- "ShadowT"]

instance ToJSON TextureType where
  toJSON v = case v of
    Texture1D t l -> object ["tag" .- "Texture1D", "texType" .= t, "layers" .= l]
    Texture2D t l -> object ["tag" .- "Texture2D", "texType" .= t, "layers" .= l]
    Texture3D t -> object ["tag" .- "Texture3D", "texType" .= t]
    TextureCube t -> object ["tag" .- "TextureCube", "texType" .= t]
    TextureRect t -> object ["tag" .- "TextureRect", "texType" .= t]
    Texture2DMS t l a b -> object ["tag" .- "Texture2DMS", "texType" .= t, "layers" .= l, "param1" .= a, "param2" .= b]
    TextureBuffer t -> object ["tag" .- "TextureBuffer", "texType" .= t]

instance ToJSON MipMap where
  toJSON v = case v of
    Mip b m -> object ["tag" .- "Mip", "base" .= b, "max" .= m]
    NoMip -> object ["tag" .- "NoMip"]
    AutoMip b m -> object ["tag" .- "AutoMip", "base" .= b, "max" .= m]

instance ToJSON Filter where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON EdgeMode where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON ImageSemantic where
  toJSON v = object ["tag" .- (pack . show $ v)]

instance ToJSON ImageRef where
  toJSON v = case v of
    TextureImage a b c -> object ["tag" .- "TextureImage", "tex" .= a, "mip" .= b, "layer" .= c]
    Framebuffer s -> object ["tag" .- "Framebuffer", "semantic" .= s]

newtype ClearImage = ClearImage (ImageSemantic,Value)
instance ToJSON ClearImage where
  toJSON (ClearImage (s,v)) = object ["semantic" .= s, "value" .= v]

instance ToJSON Command where
  toJSON v = case v of
    SetRasterContext a -> object ["tag" .- "SetRasterContext", "context" .= a]
    SetAccumulationContext a -> object ["tag" .- "SetAccumulationContext", "context" .= a]
    SetRenderTarget a -> object ["tag" .- "SetRenderTarget", "target" .= a]
    SetProgram a -> object ["tag" .- "SetProgram", "program" .= a]
    SetSamplerUniform a b -> object ["tag" .- "SetSamplerUniform", "name" .= a, "texUnit" .= b]
    SetTexture a b -> object ["tag" .- "SetTexture", "texUnit" .= a, "tex" .= b]
    SetSampler a b -> object ["tag" .- "SetSampler", "texUnit" .= a, "sampler" .= b]
    RenderSlot a -> object ["tag" .- "RenderSlot", "slot" .= a]
    ClearRenderTarget a -> object ["tag" .- "ClearRenderTarget", "values" .= fmap ClearImage a]
    GenerateMipMap a -> object ["tag" .- "GenerateMipMap", "texUnit" .= a]
    SaveImage a b -> object ["tag" .- "SaveImage", "src" .= a, "dst" .= b]
    LoadImage a b -> object ["tag" .- "SaveImage", "src" .= a, "dst" .= b]

instance ToJSON TextureDescriptor where
  toJSON TextureDescriptor{..} = object [ "textureType" .= textureType, "textureSize" .= textureSize, "textureSemantic" .= textureSemantic
                                        , "textureSampler" .= textureSampler, "textureBaseLevel" .= textureBaseLevel, "textureMaxLevel" .= textureMaxLevel]

instance ToJSON SamplerDescriptor where
  toJSON SamplerDescriptor{..} = object [ "samplerWrapS" .= samplerWrapS, "samplerWrapT" .= samplerWrapT, "samplerWrapR" .= samplerWrapR
                                        , "samplerMinFilter" .= samplerMinFilter, "samplerMagFilter" .= samplerMagFilter
                                        , "samplerBorderColor" .= samplerBorderColor , "samplerMinLod" .= samplerMinLod, "samplerMaxLod" .= samplerMaxLod
                                        , "samplerLodBias" .= samplerLodBias, "samplerCompareFunc" .= samplerCompareFunc
                                        ]

newtype Parameter = Parameter (String,InputType)
instance ToJSON Parameter where
  toJSON (Parameter (n,t)) = object ["name" .= n, "ty" .= t]

instance ToJSON Program where
  toJSON Program{..} = object [ "programUniforms" .= programUniforms, "programStreams" .= fmap Parameter programStreams, "programInTextures" .= programInTextures
                              , "programOutput" .= fmap Parameter programOutput, "vertexShader" .= vertexShader
                              , "geometryShader" .= geometryShader, "fragmentShader" .= fragmentShader
                              ]

instance ToJSON Slot where
  toJSON Slot{..} = object [ "slotName" .= slotName, "slotUniforms" .= slotUniforms, "slotStreams" .= slotStreams
                           , "slotPrimitive" .= slotPrimitive, "slotPrograms" .= slotPrograms
                           ]

newtype TargetItem = TargetItem (ImageSemantic,Maybe ImageRef)
instance ToJSON TargetItem where
  toJSON (TargetItem (s,r)) = object ["semantic" .= s, "ref" .= r]

instance ToJSON RenderTarget where
  toJSON RenderTarget{..} = object ["renderTargets" .= fmap TargetItem renderTargets]

instance ToJSON Pipeline where
  toJSON Pipeline{..} = object ["textures" .= textures, "samplers" .= samplers, "targets" .= targets, "programs" .= programs, "slots" .= slots, "commands" .= commands]

newtype MyEither = MyEither (Either (SourcePos,SourcePos,String) (Pipeline,[(SourcePos,SourcePos,String)]))
instance ToJSON MyEither where
  toJSON (MyEither e) = case e of
    Left a -> object ["tag" .- "Left", "value" .= TypeInfo a]
    Right (p,i) -> object ["tag" .- "Right", "pipeline" .= p, "infos" .= fmap TypeInfo i]

-- instances for type info
newtype TypeInfo = TypeInfo (SourcePos,SourcePos,String)
instance ToJSON TypeInfo where
  toJSON (TypeInfo (s,e,m)) = object ["startL" .= sourceLine s, "startC" .= sourceColumn s ,"endL" .= sourceLine e, "endC" .= sourceColumn e, "text" .= m]
