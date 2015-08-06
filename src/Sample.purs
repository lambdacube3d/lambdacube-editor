module Sample where

import Prelude
import Data.Maybe
import Data.StrMap (fromList)
import Data.List (toList)
import Data.Tuple

import Backend
import IR
import Mesh
import Type
import Input

--  Our vertices. Tree consecutive floats give a 3D vertex; Three consecutive vertices give a triangle.
--  A cube has 6 faces with 2 triangles each, so this makes 6*2=12 triangles, and 12*3 vertices
g_vertex_buffer_data =
    [ V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0  (-1.0) (-1.0) 1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0    1.0    1.0  1.0
    , V4   1.0  (-1.0)   1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0  (-1.0)   1.0  1.0
    , V4   1.0  (-1.0) (-1.0) 1.0
    , V4   1.0    1.0    1.0  1.0
    , V4 (-1.0) (-1.0)   1.0  1.0
    , V4   1.0  (-1.0)   1.0  1.0
    , V4   1.0    1.0    1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0) (-1.0)   1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4 (-1.0) (-1.0)   1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    , V4 (-1.0) (-1.0) (-1.0) 1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4   1.0    1.0    1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4   1.0    1.0    1.0  1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4   1.0    1.0  (-1.0) 1.0
    , V4 (-1.0)   1.0    1.0  1.0
    , V4 (-1.0)   1.0  (-1.0) 1.0
    ]

--  Two UV coordinatesfor each vertex. They were created with Blender.
g_uv_buffer_data =
    [ V2 0.0 0.0
    , V2 0.0 1.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 1.0 0.0
    , V2 0.0 0.0
    , V2 1.0 0.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 0.0 1.0
    , V2 1.0 0.0
    , V2 0.0 1.0
    , V2 1.0 1.0
    , V2 1.0 0.0
    , V2 0.0 0.0
    , V2 0.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 0.0 1.0
    , V2 0.0 0.0
    , V2 1.0 0.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 0.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 0.0 1.0
    , V2 0.0 0.0
    , V2 0.0 1.0
    , V2 1.0 1.0
    , V2 0.0 0.0
    , V2 1.0 1.0
    , V2 1.0 0.0
    ]

myCube :: Mesh
myCube =
    { attributes: fromList $ toList
        [ Tuple "position4" (A_V4F g_vertex_buffer_data)
        , Tuple "vertexUV"  (A_V2F g_uv_buffer_data)
        ]
    , primitive: P_Triangles
    , gpuData: Nothing
    }

myCube2 :: Mesh
myCube2 =
  { attributes: fromList $ toList
        [ Tuple "normal"    (A_V3F [V3 0.0 1.0 0.0,V3 0.0 1.0 0.0,V3 0.0 1.0 0.0,V3 0.0 1.0 0.0,V3 0.0 1.0 0.0,V3 0.0 1.0 0.0,V3 0.0 (-1.0) 0.0,V3 0.0 (-1.0) 0.0,V3 0.0 (-1.0) 0.0,V3 0.0 (-1.0) 0.0,V3 0.0 (-1.0) 0.0,V3 0.0 (-1.0) 0.0,V3 0.0 (-0.0) 1.0,V3 0.0 (-0.0) 1.0,V3 0.0 (-0.0) 1.0,V3 (-0.0) 0.0 1.0,V3 (-0.0) 0.0 1.0,V3 (-0.0) 0.0 1.0,V3 (-0.0) (-0.0) (-1.0),V3 (-0.0) (-0.0) (-1.0),V3 (-0.0) (-0.0) (-1.0),V3 0.0 0.0 (-1.0),V3 0.0 0.0 (-1.0),V3 0.0 0.0 (-1.0),V3 (-1.0) 0.0 0.0,V3 (-1.0) 0.0 0.0,V3 (-1.0) 0.0 0.0,V3 (-1.0) 0.0 0.0,V3 (-1.0) 0.0 0.0,V3 (-1.0) 0.0 0.0,V3 1.0 0.0 0.0,V3 1.0 0.0 0.0,V3 1.0 0.0 0.0,V3 1.0 0.0 0.0,V3 1.0 0.0 0.0,V3 1.0 0.0 0.0])
        , Tuple "position"  (A_V3F [V3 1.0 1.0 1.0,V3 (-1.0) 1.0 1.0,V3 (-1.0) 1.0 (-1.0),V3 (-1.0) 1.0 (-1.0),V3 1.0 1.0 (-1.0),V3 1.0 1.0 1.0,V3 1.0 (-1.0) (-1.0),V3 (-1.0) (-1.0) (-1.0),V3 (-1.0) (-1.0) 1.0,V3 (-1.0) (-1.0) 1.0,V3 1.0 (-1.0) 1.0,V3 1.0 (-1.0) (-1.0),V3 1.0 (-1.0) 1.0,V3 (-1.0) (-1.0) 1.0,V3 (-1.0) 1.0 1.0,V3 (-1.0) 1.0 1.0,V3 1.0 1.0 1.0,V3 1.0 (-1.0) 1.0,V3 1.0 1.0 (-1.0),V3 (-1.0) 1.0 (-1.0),V3 (-1.0) (-1.0) (-1.0),V3 (-1.0) (-1.0) (-1.0),V3 1.0 (-1.0) (-1.0),V3 1.0 1.0 (-1.0),V3 (-1.0) (-1.0) 1.0,V3 (-1.0) (-1.0) (-1.0),V3 (-1.0) 1.0 (-1.0),V3 (-1.0) 1.0 (-1.0),V3 (-1.0) 1.0 1.0,V3 (-1.0) (-1.0) 1.0,V3 1.0 (-1.0) (-1.0),V3 1.0 (-1.0) 1.0,V3 1.0 1.0 1.0,V3 1.0 1.0 1.0,V3 1.0 1.0 (-1.0),V3 1.0 (-1.0) (-1.0)])
        ]
  , primitive: P_Triangles
  , gpuData: Nothing
  }

myQuad :: Mesh
myQuad =
  { attributes: fromList $ toList
      [ Tuple "position" (A_V2F [V2 (-1.0) 1.0,V2 (-1.0) (-1.0),V2 1.0 (-1.0),V2 1.0 (-1.0),V2 1.0 1.0,V2 (-1.0) 1.0])]
  , primitive: P_Triangles
  , gpuData: Nothing
  }
