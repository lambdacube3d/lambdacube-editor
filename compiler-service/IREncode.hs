-- generated file, do not modify!
-- 2015-09-09T11:09:31.728951000000Z

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module IREncode where

import Data.Text
import Data.Aeson hiding (Value,Bool)
import Data.Aeson.Types hiding (Value,Bool)
import Linear

import IR

(.-) :: Text -> Text -> Pair
a .- b = a .= b

instance ToJSON ArrayValue where
  toJSON v = case v of
    VBoolArray arg0 -> object [ "tag" .- "VBoolArray", "arg0" .= arg0]
    VIntArray arg0 -> object [ "tag" .- "VIntArray", "arg0" .= arg0]
    VWordArray arg0 -> object [ "tag" .- "VWordArray", "arg0" .= arg0]
    VFloatArray arg0 -> object [ "tag" .- "VFloatArray", "arg0" .= arg0]

instance ToJSON Value where
  toJSON v = case v of
    VBool arg0 -> object [ "tag" .- "VBool", "arg0" .= arg0]
    VV2B arg0 -> object [ "tag" .- "VV2B", "arg0" .= arg0]
    VV3B arg0 -> object [ "tag" .- "VV3B", "arg0" .= arg0]
    VV4B arg0 -> object [ "tag" .- "VV4B", "arg0" .= arg0]
    VWord arg0 -> object [ "tag" .- "VWord", "arg0" .= arg0]
    VV2U arg0 -> object [ "tag" .- "VV2U", "arg0" .= arg0]
    VV3U arg0 -> object [ "tag" .- "VV3U", "arg0" .= arg0]
    VV4U arg0 -> object [ "tag" .- "VV4U", "arg0" .= arg0]
    VInt arg0 -> object [ "tag" .- "VInt", "arg0" .= arg0]
    VV2I arg0 -> object [ "tag" .- "VV2I", "arg0" .= arg0]
    VV3I arg0 -> object [ "tag" .- "VV3I", "arg0" .= arg0]
    VV4I arg0 -> object [ "tag" .- "VV4I", "arg0" .= arg0]
    VFloat arg0 -> object [ "tag" .- "VFloat", "arg0" .= arg0]
    VV2F arg0 -> object [ "tag" .- "VV2F", "arg0" .= arg0]
    VV3F arg0 -> object [ "tag" .- "VV3F", "arg0" .= arg0]
    VV4F arg0 -> object [ "tag" .- "VV4F", "arg0" .= arg0]
    VM22F arg0 -> object [ "tag" .- "VM22F", "arg0" .= arg0]
    VM23F arg0 -> object [ "tag" .- "VM23F", "arg0" .= arg0]
    VM24F arg0 -> object [ "tag" .- "VM24F", "arg0" .= arg0]
    VM32F arg0 -> object [ "tag" .- "VM32F", "arg0" .= arg0]
    VM33F arg0 -> object [ "tag" .- "VM33F", "arg0" .= arg0]
    VM34F arg0 -> object [ "tag" .- "VM34F", "arg0" .= arg0]
    VM42F arg0 -> object [ "tag" .- "VM42F", "arg0" .= arg0]
    VM43F arg0 -> object [ "tag" .- "VM43F", "arg0" .= arg0]
    VM44F arg0 -> object [ "tag" .- "VM44F", "arg0" .= arg0]

instance ToJSON InputType where
  toJSON v = case v of
    Bool -> object [ "tag" .- "Bool"]
    V2B -> object [ "tag" .- "V2B"]
    V3B -> object [ "tag" .- "V3B"]
    V4B -> object [ "tag" .- "V4B"]
    Word -> object [ "tag" .- "Word"]
    V2U -> object [ "tag" .- "V2U"]
    V3U -> object [ "tag" .- "V3U"]
    V4U -> object [ "tag" .- "V4U"]
    Int -> object [ "tag" .- "Int"]
    V2I -> object [ "tag" .- "V2I"]
    V3I -> object [ "tag" .- "V3I"]
    V4I -> object [ "tag" .- "V4I"]
    Float -> object [ "tag" .- "Float"]
    V2F -> object [ "tag" .- "V2F"]
    V3F -> object [ "tag" .- "V3F"]
    V4F -> object [ "tag" .- "V4F"]
    M22F -> object [ "tag" .- "M22F"]
    M23F -> object [ "tag" .- "M23F"]
    M24F -> object [ "tag" .- "M24F"]
    M32F -> object [ "tag" .- "M32F"]
    M33F -> object [ "tag" .- "M33F"]
    M34F -> object [ "tag" .- "M34F"]
    M42F -> object [ "tag" .- "M42F"]
    M43F -> object [ "tag" .- "M43F"]
    M44F -> object [ "tag" .- "M44F"]
    STexture1D -> object [ "tag" .- "STexture1D"]
    STexture2D -> object [ "tag" .- "STexture2D"]
    STextureCube -> object [ "tag" .- "STextureCube"]
    STexture1DArray -> object [ "tag" .- "STexture1DArray"]
    STexture2DArray -> object [ "tag" .- "STexture2DArray"]
    STexture2DRect -> object [ "tag" .- "STexture2DRect"]
    FTexture1D -> object [ "tag" .- "FTexture1D"]
    FTexture2D -> object [ "tag" .- "FTexture2D"]
    FTexture3D -> object [ "tag" .- "FTexture3D"]
    FTextureCube -> object [ "tag" .- "FTextureCube"]
    FTexture1DArray -> object [ "tag" .- "FTexture1DArray"]
    FTexture2DArray -> object [ "tag" .- "FTexture2DArray"]
    FTexture2DMS -> object [ "tag" .- "FTexture2DMS"]
    FTexture2DMSArray -> object [ "tag" .- "FTexture2DMSArray"]
    FTextureBuffer -> object [ "tag" .- "FTextureBuffer"]
    FTexture2DRect -> object [ "tag" .- "FTexture2DRect"]
    ITexture1D -> object [ "tag" .- "ITexture1D"]
    ITexture2D -> object [ "tag" .- "ITexture2D"]
    ITexture3D -> object [ "tag" .- "ITexture3D"]
    ITextureCube -> object [ "tag" .- "ITextureCube"]
    ITexture1DArray -> object [ "tag" .- "ITexture1DArray"]
    ITexture2DArray -> object [ "tag" .- "ITexture2DArray"]
    ITexture2DMS -> object [ "tag" .- "ITexture2DMS"]
    ITexture2DMSArray -> object [ "tag" .- "ITexture2DMSArray"]
    ITextureBuffer -> object [ "tag" .- "ITextureBuffer"]
    ITexture2DRect -> object [ "tag" .- "ITexture2DRect"]
    UTexture1D -> object [ "tag" .- "UTexture1D"]
    UTexture2D -> object [ "tag" .- "UTexture2D"]
    UTexture3D -> object [ "tag" .- "UTexture3D"]
    UTextureCube -> object [ "tag" .- "UTextureCube"]
    UTexture1DArray -> object [ "tag" .- "UTexture1DArray"]
    UTexture2DArray -> object [ "tag" .- "UTexture2DArray"]
    UTexture2DMS -> object [ "tag" .- "UTexture2DMS"]
    UTexture2DMSArray -> object [ "tag" .- "UTexture2DMSArray"]
    UTextureBuffer -> object [ "tag" .- "UTextureBuffer"]
    UTexture2DRect -> object [ "tag" .- "UTexture2DRect"]

instance ToJSON PointSpriteCoordOrigin where
  toJSON v = case v of
    LowerLeft -> object [ "tag" .- "LowerLeft"]
    UpperLeft -> object [ "tag" .- "UpperLeft"]

instance ToJSON PointSize where
  toJSON v = case v of
    PointSize arg0 -> object [ "tag" .- "PointSize", "arg0" .= arg0]
    ProgramPointSize -> object [ "tag" .- "ProgramPointSize"]

instance ToJSON PolygonOffset where
  toJSON v = case v of
    NoOffset -> object [ "tag" .- "NoOffset"]
    Offset arg0 arg1 -> object [ "tag" .- "Offset", "arg0" .= arg0, "arg1" .= arg1]

instance ToJSON FrontFace where
  toJSON v = case v of
    CCW -> object [ "tag" .- "CCW"]
    CW -> object [ "tag" .- "CW"]

instance ToJSON PolygonMode where
  toJSON v = case v of
    PolygonPoint arg0 -> object [ "tag" .- "PolygonPoint", "arg0" .= arg0]
    PolygonLine arg0 -> object [ "tag" .- "PolygonLine", "arg0" .= arg0]
    PolygonFill -> object [ "tag" .- "PolygonFill"]

instance ToJSON ProvokingVertex where
  toJSON v = case v of
    FirstVertex -> object [ "tag" .- "FirstVertex"]
    LastVertex -> object [ "tag" .- "LastVertex"]

instance ToJSON CullMode where
  toJSON v = case v of
    CullNone -> object [ "tag" .- "CullNone"]
    CullFront arg0 -> object [ "tag" .- "CullFront", "arg0" .= arg0]
    CullBack arg0 -> object [ "tag" .- "CullBack", "arg0" .= arg0]

instance ToJSON ComparisonFunction where
  toJSON v = case v of
    Never -> object [ "tag" .- "Never"]
    Less -> object [ "tag" .- "Less"]
    Equal -> object [ "tag" .- "Equal"]
    Lequal -> object [ "tag" .- "Lequal"]
    Greater -> object [ "tag" .- "Greater"]
    Notequal -> object [ "tag" .- "Notequal"]
    Gequal -> object [ "tag" .- "Gequal"]
    Always -> object [ "tag" .- "Always"]

instance ToJSON StencilOperation where
  toJSON v = case v of
    OpZero -> object [ "tag" .- "OpZero"]
    OpKeep -> object [ "tag" .- "OpKeep"]
    OpReplace -> object [ "tag" .- "OpReplace"]
    OpIncr -> object [ "tag" .- "OpIncr"]
    OpIncrWrap -> object [ "tag" .- "OpIncrWrap"]
    OpDecr -> object [ "tag" .- "OpDecr"]
    OpDecrWrap -> object [ "tag" .- "OpDecrWrap"]
    OpInvert -> object [ "tag" .- "OpInvert"]

instance ToJSON BlendEquation where
  toJSON v = case v of
    FuncAdd -> object [ "tag" .- "FuncAdd"]
    FuncSubtract -> object [ "tag" .- "FuncSubtract"]
    FuncReverseSubtract -> object [ "tag" .- "FuncReverseSubtract"]
    Min -> object [ "tag" .- "Min"]
    Max -> object [ "tag" .- "Max"]

instance ToJSON BlendingFactor where
  toJSON v = case v of
    Zero -> object [ "tag" .- "Zero"]
    One -> object [ "tag" .- "One"]
    SrcColor -> object [ "tag" .- "SrcColor"]
    OneMinusSrcColor -> object [ "tag" .- "OneMinusSrcColor"]
    DstColor -> object [ "tag" .- "DstColor"]
    OneMinusDstColor -> object [ "tag" .- "OneMinusDstColor"]
    SrcAlpha -> object [ "tag" .- "SrcAlpha"]
    OneMinusSrcAlpha -> object [ "tag" .- "OneMinusSrcAlpha"]
    DstAlpha -> object [ "tag" .- "DstAlpha"]
    OneMinusDstAlpha -> object [ "tag" .- "OneMinusDstAlpha"]
    ConstantColor -> object [ "tag" .- "ConstantColor"]
    OneMinusConstantColor -> object [ "tag" .- "OneMinusConstantColor"]
    ConstantAlpha -> object [ "tag" .- "ConstantAlpha"]
    OneMinusConstantAlpha -> object [ "tag" .- "OneMinusConstantAlpha"]
    SrcAlphaSaturate -> object [ "tag" .- "SrcAlphaSaturate"]

instance ToJSON LogicOperation where
  toJSON v = case v of
    Clear -> object [ "tag" .- "Clear"]
    And -> object [ "tag" .- "And"]
    AndReverse -> object [ "tag" .- "AndReverse"]
    Copy -> object [ "tag" .- "Copy"]
    AndInverted -> object [ "tag" .- "AndInverted"]
    Noop -> object [ "tag" .- "Noop"]
    Xor -> object [ "tag" .- "Xor"]
    Or -> object [ "tag" .- "Or"]
    Nor -> object [ "tag" .- "Nor"]
    Equiv -> object [ "tag" .- "Equiv"]
    Invert -> object [ "tag" .- "Invert"]
    OrReverse -> object [ "tag" .- "OrReverse"]
    CopyInverted -> object [ "tag" .- "CopyInverted"]
    OrInverted -> object [ "tag" .- "OrInverted"]
    Nand -> object [ "tag" .- "Nand"]
    Set -> object [ "tag" .- "Set"]

instance ToJSON StencilOps where
  toJSON v = case v of
    StencilOps{..} -> object
      [ "tag" .- "StencilOps"
      , "frontStencilOp" .= frontStencilOp
      , "backStencilOp" .= backStencilOp
      ]

instance ToJSON StencilTests where
  toJSON v = case v of
    StencilTests arg0 arg1 -> object [ "tag" .- "StencilTests", "arg0" .= arg0, "arg1" .= arg1]

instance ToJSON StencilTest where
  toJSON v = case v of
    StencilTest{..} -> object
      [ "tag" .- "StencilTest"
      , "stencilComparision" .= stencilComparision
      , "stencilReference" .= stencilReference
      , "stencilMask" .= stencilMask
      ]

instance ToJSON FetchPrimitive where
  toJSON v = case v of
    Points -> object [ "tag" .- "Points"]
    Lines -> object [ "tag" .- "Lines"]
    Triangles -> object [ "tag" .- "Triangles"]
    LinesAdjacency -> object [ "tag" .- "LinesAdjacency"]
    TrianglesAdjacency -> object [ "tag" .- "TrianglesAdjacency"]

instance ToJSON OutputPrimitive where
  toJSON v = case v of
    TrianglesOutput -> object [ "tag" .- "TrianglesOutput"]
    LinesOutput -> object [ "tag" .- "LinesOutput"]
    PointsOutput -> object [ "tag" .- "PointsOutput"]

instance ToJSON ColorArity where
  toJSON v = case v of
    Red -> object [ "tag" .- "Red"]
    RG -> object [ "tag" .- "RG"]
    RGB -> object [ "tag" .- "RGB"]
    RGBA -> object [ "tag" .- "RGBA"]

instance ToJSON Blending where
  toJSON v = case v of
    NoBlending -> object [ "tag" .- "NoBlending"]
    BlendLogicOp arg0 -> object [ "tag" .- "BlendLogicOp", "arg0" .= arg0]
    Blend{..} -> object
      [ "tag" .- "Blend"
      , "colorEqSrc" .= colorEqSrc
      , "alphaEqSrc" .= alphaEqSrc
      , "colorFSrc" .= colorFSrc
      , "colorFDst" .= colorFDst
      , "alphaFSrc" .= alphaFSrc
      , "alphaFDst" .= alphaFDst
      , "color" .= color
      ]

instance ToJSON RasterContext where
  toJSON v = case v of
    PointCtx arg0 arg1 arg2 -> object [ "tag" .- "PointCtx", "arg0" .= arg0, "arg1" .= arg1, "arg2" .= arg2]
    LineCtx arg0 arg1 -> object [ "tag" .- "LineCtx", "arg0" .= arg0, "arg1" .= arg1]
    TriangleCtx arg0 arg1 arg2 arg3 -> object [ "tag" .- "TriangleCtx", "arg0" .= arg0, "arg1" .= arg1, "arg2" .= arg2, "arg3" .= arg3]

instance ToJSON FragmentOperation where
  toJSON v = case v of
    DepthOp arg0 arg1 -> object [ "tag" .- "DepthOp", "arg0" .= arg0, "arg1" .= arg1]
    StencilOp arg0 arg1 arg2 -> object [ "tag" .- "StencilOp", "arg0" .= arg0, "arg1" .= arg1, "arg2" .= arg2]
    ColorOp arg0 arg1 -> object [ "tag" .- "ColorOp", "arg0" .= arg0, "arg1" .= arg1]

instance ToJSON AccumulationContext where
  toJSON v = case v of
    AccumulationContext{..} -> object
      [ "tag" .- "AccumulationContext"
      , "accViewportName" .= accViewportName
      , "accOperations" .= accOperations
      ]

instance ToJSON TextureDataType where
  toJSON v = case v of
    FloatT arg0 -> object [ "tag" .- "FloatT", "arg0" .= arg0]
    IntT arg0 -> object [ "tag" .- "IntT", "arg0" .= arg0]
    WordT arg0 -> object [ "tag" .- "WordT", "arg0" .= arg0]
    ShadowT -> object [ "tag" .- "ShadowT"]

instance ToJSON TextureType where
  toJSON v = case v of
    Texture1D arg0 arg1 -> object [ "tag" .- "Texture1D", "arg0" .= arg0, "arg1" .= arg1]
    Texture2D arg0 arg1 -> object [ "tag" .- "Texture2D", "arg0" .= arg0, "arg1" .= arg1]
    Texture3D arg0 -> object [ "tag" .- "Texture3D", "arg0" .= arg0]
    TextureCube arg0 -> object [ "tag" .- "TextureCube", "arg0" .= arg0]
    TextureRect arg0 -> object [ "tag" .- "TextureRect", "arg0" .= arg0]
    Texture2DMS arg0 arg1 arg2 arg3 -> object [ "tag" .- "Texture2DMS", "arg0" .= arg0, "arg1" .= arg1, "arg2" .= arg2, "arg3" .= arg3]
    TextureBuffer arg0 -> object [ "tag" .- "TextureBuffer", "arg0" .= arg0]

instance ToJSON MipMap where
  toJSON v = case v of
    Mip arg0 arg1 -> object [ "tag" .- "Mip", "arg0" .= arg0, "arg1" .= arg1]
    NoMip -> object [ "tag" .- "NoMip"]
    AutoMip arg0 arg1 -> object [ "tag" .- "AutoMip", "arg0" .= arg0, "arg1" .= arg1]

instance ToJSON Filter where
  toJSON v = case v of
    Nearest -> object [ "tag" .- "Nearest"]
    Linear -> object [ "tag" .- "Linear"]
    NearestMipmapNearest -> object [ "tag" .- "NearestMipmapNearest"]
    NearestMipmapLinear -> object [ "tag" .- "NearestMipmapLinear"]
    LinearMipmapNearest -> object [ "tag" .- "LinearMipmapNearest"]
    LinearMipmapLinear -> object [ "tag" .- "LinearMipmapLinear"]

instance ToJSON EdgeMode where
  toJSON v = case v of
    Repeat -> object [ "tag" .- "Repeat"]
    MirroredRepeat -> object [ "tag" .- "MirroredRepeat"]
    ClampToEdge -> object [ "tag" .- "ClampToEdge"]
    ClampToBorder -> object [ "tag" .- "ClampToBorder"]

instance ToJSON ImageRef where
  toJSON v = case v of
    TextureImage arg0 arg1 arg2 -> object [ "tag" .- "TextureImage", "arg0" .= arg0, "arg1" .= arg1, "arg2" .= arg2]
    Framebuffer arg0 -> object [ "tag" .- "Framebuffer", "arg0" .= arg0]

instance ToJSON ImageSemantic where
  toJSON v = case v of
    Depth -> object [ "tag" .- "Depth"]
    Stencil -> object [ "tag" .- "Stencil"]
    Color -> object [ "tag" .- "Color"]

instance ToJSON ClearImage where
  toJSON v = case v of
    ClearImage{..} -> object
      [ "tag" .- "ClearImage"
      , "imageSemantic" .= imageSemantic
      , "clearValue" .= clearValue
      ]

instance ToJSON Command where
  toJSON v = case v of
    SetRasterContext arg0 -> object [ "tag" .- "SetRasterContext", "arg0" .= arg0]
    SetAccumulationContext arg0 -> object [ "tag" .- "SetAccumulationContext", "arg0" .= arg0]
    SetRenderTarget arg0 -> object [ "tag" .- "SetRenderTarget", "arg0" .= arg0]
    SetProgram arg0 -> object [ "tag" .- "SetProgram", "arg0" .= arg0]
    SetSamplerUniform arg0 arg1 -> object [ "tag" .- "SetSamplerUniform", "arg0" .= arg0, "arg1" .= arg1]
    SetTexture arg0 arg1 -> object [ "tag" .- "SetTexture", "arg0" .= arg0, "arg1" .= arg1]
    SetSampler arg0 arg1 -> object [ "tag" .- "SetSampler", "arg0" .= arg0, "arg1" .= arg1]
    RenderSlot arg0 -> object [ "tag" .- "RenderSlot", "arg0" .= arg0]
    RenderStream arg0 -> object [ "tag" .- "RenderStream", "arg0" .= arg0]
    ClearRenderTarget arg0 -> object [ "tag" .- "ClearRenderTarget", "arg0" .= arg0]
    GenerateMipMap arg0 -> object [ "tag" .- "GenerateMipMap", "arg0" .= arg0]
    SaveImage arg0 arg1 -> object [ "tag" .- "SaveImage", "arg0" .= arg0, "arg1" .= arg1]
    LoadImage arg0 arg1 -> object [ "tag" .- "LoadImage", "arg0" .= arg0, "arg1" .= arg1]

instance ToJSON TextureDescriptor where
  toJSON v = case v of
    TextureDescriptor{..} -> object
      [ "tag" .- "TextureDescriptor"
      , "textureType" .= textureType
      , "textureSize" .= textureSize
      , "textureSemantic" .= textureSemantic
      , "textureSampler" .= textureSampler
      , "textureBaseLevel" .= textureBaseLevel
      , "textureMaxLevel" .= textureMaxLevel
      ]

instance ToJSON SamplerDescriptor where
  toJSON v = case v of
    SamplerDescriptor{..} -> object
      [ "tag" .- "SamplerDescriptor"
      , "samplerWrapS" .= samplerWrapS
      , "samplerWrapT" .= samplerWrapT
      , "samplerWrapR" .= samplerWrapR
      , "samplerMinFilter" .= samplerMinFilter
      , "samplerMagFilter" .= samplerMagFilter
      , "samplerBorderColor" .= samplerBorderColor
      , "samplerMinLod" .= samplerMinLod
      , "samplerMaxLod" .= samplerMaxLod
      , "samplerLodBias" .= samplerLodBias
      , "samplerCompareFunc" .= samplerCompareFunc
      ]

instance ToJSON Parameter where
  toJSON v = case v of
    Parameter{..} -> object
      [ "tag" .- "Parameter"
      , "name" .= name
      , "ty" .= ty
      ]

instance ToJSON Program where
  toJSON v = case v of
    Program{..} -> object
      [ "tag" .- "Program"
      , "programUniforms" .= programUniforms
      , "programStreams" .= programStreams
      , "programInTextures" .= programInTextures
      , "programOutput" .= programOutput
      , "vertexShader" .= vertexShader
      , "geometryShader" .= geometryShader
      , "fragmentShader" .= fragmentShader
      ]

instance ToJSON Slot where
  toJSON v = case v of
    Slot{..} -> object
      [ "tag" .- "Slot"
      , "slotName" .= slotName
      , "slotStreams" .= slotStreams
      , "slotUniforms" .= slotUniforms
      , "slotPrimitive" .= slotPrimitive
      , "slotPrograms" .= slotPrograms
      ]

instance ToJSON StreamData where
  toJSON v = case v of
    StreamData{..} -> object
      [ "tag" .- "StreamData"
      , "streamData" .= streamData
      , "streamType" .= streamType
      , "streamPrimitive" .= streamPrimitive
      , "streamPrograms" .= streamPrograms
      ]

instance ToJSON TargetItem where
  toJSON v = case v of
    TargetItem{..} -> object
      [ "tag" .- "TargetItem"
      , "targetSemantic" .= targetSemantic
      , "targetRef" .= targetRef
      ]

instance ToJSON RenderTarget where
  toJSON v = case v of
    RenderTarget{..} -> object
      [ "tag" .- "RenderTarget"
      , "renderTargets" .= renderTargets
      ]

instance ToJSON Backend where
  toJSON v = case v of
    WebGL1 -> object [ "tag" .- "WebGL1"]
    OpenGL33 -> object [ "tag" .- "OpenGL33"]

instance ToJSON Pipeline where
  toJSON v = case v of
    Pipeline{..} -> object
      [ "tag" .- "Pipeline"
      , "backend" .= backend
      , "textures" .= textures
      , "samplers" .= samplers
      , "targets" .= targets
      , "programs" .= programs
      , "slots" .= slots
      , "streams" .= streams
      , "commands" .= commands
      ]

