module PureGL.Renderer where

import Prelude

import Control.Monad.State (get)
import PureGL.Geometry (LoadedGeometry(..))
import PureGL.Program (LoadedProgram(..))
import PureGL.RenderResource (lookupResource)
import PureGL.RenderState (RenderT)
import PureGL.Types (ResourceId)
import PureGL.WebGL (bindVertexArray, clear, clearColor, drawArrays, useProgram)
import PureGL.WebGL.Constants (gl_COLOR_BUFFER_BIT, gl_TRIANGLES)

renderGeometry :: forall eff. ResourceId -> ResourceId -> RenderT eff Unit
renderGeometry geometryId programId = do
  state <- get
  (LoadedGeometry g) <- lookupResource geometryId
  (LoadedProgram p) <- lookupResource programId
  useProgram p.program
  bindVertexArray g.vao
  clearColor 0.0 0.0 0.0 1.0
  clear gl_COLOR_BUFFER_BIT
  drawArrays gl_TRIANGLES 0 g.vertexCount

