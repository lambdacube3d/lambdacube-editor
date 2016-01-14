module DefaultText where

defaultSrc = """

f x = (x + sin x + sin (1.1 * x)) `mod` 4.0 * 2

makeFrame (time :: Float)
          (projmat :: Mat 4 4 Float)
          (vertexstream :: VertexStream Triangle (Vec 4 Float))

    = imageFrame (emptyDepthImage 1000, emptyColorImage navy)
  `overlay`
      vertexstream
    & transformVertices (scale 0.5 . (projmat *.))
    & rasterize (TriangleCtx CullNone PolygonFill NoOffset LastVertex)
    & filterFragmentStream PassAll
    & transformFragmentsRastDepth (\x -> (rotMatrixZ time *. rotMatrixY time *. x) *! f time)
    & accumulateWith (DepthOp Less True, ColorOp NoBlending (V4 True True True True))

main = renderFrame $
   makeFrame (Uniform "Time")
             (Uniform "MVP")
             (Fetch "stream4" (Attribute "position4"))

"""
