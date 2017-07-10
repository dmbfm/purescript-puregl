module PureGL.Geometry where

import Prelude

import PureGL.Context (ContextR)
import PureGL.VertexBuffer (VertexBuffer)
import PureGL.WebGL.Types (WebGLVertexArrayObject)

newtype Geometry = Geometry { vertexBuffers :: Array VertexBuffer
                            , id :: Int
                            }

newtype GeometryResource = GeometryResource { vao :: WebGLVertexArrayObject
                                            , update :: Boolean
                                            }

instance geometryShow :: Show Geometry where
  show (Geometry g) = 
       "Geometry: {"
    <> "vertexBuffers: " <> (show g.vertexBuffers) 
    <> ", id: " <> (show g.id)
    <> "}"

mkGeometry :: Array VertexBuffer -> Int -> Geometry
mkGeometry vbs id = Geometry { vertexBuffers: vbs 
                             , id: id
                             }

loadGeometryResource :: forall eff. Geometry -> ContextR eff (Either String GeometryResource)
loadGeometryResource g = do
  
