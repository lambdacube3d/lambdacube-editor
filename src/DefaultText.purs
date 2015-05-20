module DefaultText where

defaultSrc = """
blue = V4F 0 0 1 1
red  = V4F 1 0 0 1
clear = FrameBuffer $ ColorImage @1 red   -- ...

triangleRasterCtx = TriangleCtx CullNone PolygonFill NoOffset LastVertex
colorFragmentCtx = AccumulationContext (ColorOp NoBlending (V4B True True True True))

rasterizeWith = Rasterize
triangles = triangleRasterCtx

cubeVertexStream = Fetch "stream4" Triangles (IV4F "position4")
mapFragments s fs = Accumulate colorFragmentCtx PassAll (\a -> FragmentOut $ fs a) s clear
transform s f =  Transform (\v -> VertexOut (f v) 1 () (Smooth v)) s

scale t v = v `PrimMul` V4 t t t 1

rotate v = (Uni $ IM44F "MVP") `PrimMulMatVec` v

const x y = x
x & f = f x



main =             cubeVertexStream         -- cube vertices
    `transform`    (scale 0.5 . rotate)     -- scale and rotate
     &             rasterizeWith triangles  -- rasterize
    `mapFragments` const blue               -- set every pixel blue
     &             ScreenOut                --  draw into screen
"""
