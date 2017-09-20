module PureGL.Renderer.RenderResource where

import Prelude
import PureGL.WebGL
import Control.Monad.Error.Class (throwError)
import Control.Monad.State (modify)
import Data.Array (mapWithIndex, unsafeIndex)
import Data.Dynamic (toDynamic)
import Data.Lens (set, view)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)
import PureGL.Renderer.Framebuffer (FBColorAttachment(..), Framebuffer(..), LoadedFramebuffer(..), LoadedRenderbuffer(..), Renderbuffer(..))
import PureGL.Renderer.GLConstant (getValue)
import PureGL.Renderer.Geometry (Geometry(..), LoadedGeometry(..), VertexAttribute(..))
import PureGL.Renderer.Internal.Framebuffer (attachToFramebuffer)
import PureGL.Renderer.Internal.Program (buildProgram, getProgramAttribLocationsMap, getProgramUniformLocationsMap, setUniformsMap)
import PureGL.Renderer.Internal.Texture (setSamplerState)
import PureGL.Renderer.Program (LoadedProgram(..), Program(..))
import PureGL.Renderer.RenderState (RenderError(..), RenderState, RenderT, _loadedFramebuffers, _loadedGeometries, _loadedPrograms, _loadedRenderbuffers, _loadedTextures, _renderState, _renderer', addLoadedFramebuffer, addLoadedGeometry, addLoadedProgram, addLoadedRenderbuffer, addLoadedTexture, getRenderer)
import PureGL.Renderer.Texture (LoadedTexture(..), RenderTexture(..), Texture(..), TextureFormat(..), TexturePixels(..))
import PureGL.Renderer.Types (ResourceId)
import PureGL.Utils.HasID (requestId)
import PureGL.WebGL.Constants (gl_ARRAY_BUFFER, gl_COLOR_ATTACHMENT0, gl_DEPTH_ATTACHMENT, gl_FLOAT, gl_FRAMEBUFFER, gl_RENDERBUFFER, gl_STATIC_DRAW)
import PureGL.WebGL.Raw (glNullBufferObject, glNullFramebufferObject, glNullVertexArrayObject)
  
class InsertResource a where
  insertResource :: forall e r. a -> RenderT r e ResourceId  
  lookupResource :: forall e r. ResourceId -> RenderT r e a

class LoadResource a where
  loadResource :: forall e r. a -> RenderT r e ResourceId

instance loadedGeometryInsertResource :: InsertResource LoadedGeometry where
  insertResource r = _insertResource r addLoadedGeometry
  lookupResource id = _lookupResource id (view $ _renderState <<< _loadedGeometries)

instance insertResourceLoadedTexture :: InsertResource LoadedTexture where
  insertResource r = _insertResource r addLoadedTexture
  lookupResource id = _lookupResource id (view $ _renderState <<< _loadedTextures)

instance insertResourceLoadedRenderbuffer :: InsertResource LoadedRenderbuffer where
  insertResource r = _insertResource r addLoadedRenderbuffer
  lookupResource id = _lookupResource id (view $ _renderState <<< _loadedRenderbuffers)

instance insertResourceLoadedFramebuffer :: InsertResource LoadedFramebuffer where
  insertResource r = _insertResource r addLoadedFramebuffer
  lookupResource id = _lookupResource id (view $ _renderState <<< _loadedFramebuffers)

instance insertResourceLoadedProgram :: InsertResource LoadedProgram where
  insertResource r = _insertResource r addLoadedProgram
  lookupResource id = _lookupResource id (view $ _renderState <<< _loadedPrograms)


_insertResource :: forall r e a. a -> (Int -> a -> RenderState -> RenderState) -> RenderT r e ResourceId
_insertResource res f = do
  renderState <- getRenderer
  case requestId renderState of
    Tuple id renderState' -> do
      modify $ set _renderer' $ f id res renderState'
      pure id

_lookupResource :: forall r e a. ResourceId -> (RenderState -> Map ResourceId a) -> RenderT r e a
_lookupResource id f = do
  renderState <- getRenderer
  case lookup id (f renderState) of
    Just r -> pure r
    Nothing -> throwError (toDynamic $ LookupResourceError id)

instance loadResourceGeometry :: LoadResource Geometry where
  loadResource = loadGeometry    

instance loadResourceProgram :: LoadResource Program where
  loadResource = loadProgram

instance loadResourceTexture :: LoadResource Texture where
  loadResource = loadTexture

instance loadResourceRenderTexture :: LoadResource RenderTexture where
  loadResource = loadRenderTexture

instance loadResourceRenderbuffer :: LoadResource Renderbuffer where
  loadResource = loadRenderbuffer

instance loadResourceFramebuffer :: LoadResource Framebuffer where
  loadResource = loadFramebuffer

loadGeometry :: forall r e.  Geometry -> RenderT r e ResourceId
loadGeometry (Geometry g) = do
  buffer <- createBuffer
  vao <- createVertexArray
  bindVertexArray vao
  bindBuffer gl_ARRAY_BUFFER buffer
  bufferData gl_ARRAY_BUFFER g.vertexData gl_STATIC_DRAW
  _ <- sequence $ mapWithIndex (\idx attr -> 
        case attr of 
          FloatAttribute size -> do
            vertexAttribPointer idx size gl_FLOAT false g.vertexSize (unsafePartial $ unsafeIndex g.offsets idx)
            enableVertexAttribArray idx
      ) g.attributes
  --bindVertexArray glNullVertexArrayObject
  --bindBuffer gl_ARRAY_BUFFER glNullBufferObject
  insertResource $  LoadedGeometry { buffer: buffer, vao: vao, vertexCount: g.vertexCount }

loadProgram :: forall r e. Program -> RenderT r e ResourceId
loadProgram (Program p) = do
  program <- buildProgram p.vertexShaderSource p.fragmentShaderSource
  uniformLocs <- getProgramUniformLocationsMap program p.uniforms
  attrLocs <- getProgramAttribLocationsMap program p.attributes
  useProgram program
  setUniformsMap uniformLocs p.uniforms
  insertResource $ LoadedProgram { program: program
                                 , uniformLocations: uniformLocs
                                 , attributeLocations: attrLocs
                                 }

loadTexture :: forall e r. Texture -> RenderT e r ResourceId
loadTexture (Texture t) = do
  texture <- createTexture
  let target = getValue t.textureTarget
  bindTexture target texture
  let (TextureFormat fmt) = t.format
  setSamplerState texture target t.sampler
  case t.pixels of
    (HTMLImagePixels p) -> texImage2D target 0 (getValue fmt.internalFormat) (getValue fmt.format) (getValue fmt.texelDataType) p
    (HTMLVideoPixels p) -> texImage2D target 0 (getValue fmt.internalFormat) (getValue fmt.format) (getValue fmt.texelDataType) p
    (HTMLCanvasPixels p) -> texImage2D target 0 (getValue fmt.internalFormat) (getValue fmt.format) (getValue fmt.texelDataType) p
    (ImageDataPixels p) -> texImage2D target 0 (getValue fmt.internalFormat) (getValue fmt.format) (getValue fmt.texelDataType) p
    (Uint8ArrayPixels p) -> texImage2D target 0 (getValue fmt.internalFormat) (getValue fmt.format) (getValue fmt.texelDataType) p
  generateMipmap target
  insertResource $ LoadedTexture { texture: texture
                                 , textureTarget: t.textureTarget
                                 }

loadRenderTexture :: forall e a. RenderTexture -> RenderT e a ResourceId
loadRenderTexture (RenderTexture t) = do
  texture <- createTexture
  let target = getValue (t.textureTarget)
  bindTexture target texture
  let (TextureFormat fmt) = t.format
  setSamplerState texture target t.sampler
  texImage2D' target 0 (getValue fmt.internalFormat) t.size.width t.size.height (getValue fmt.format) (getValue fmt.texelDataType)
  insertResource $ LoadedTexture { texture: texture
                                 , textureTarget: t.textureTarget
                                 }

loadRenderbuffer :: forall e r. Renderbuffer -> RenderT e r ResourceId
loadRenderbuffer (Renderbuffer rb) = do
  renderbuffer <- createRenderbuffer
  bindRenderbuffer gl_RENDERBUFFER renderbuffer
  renderbufferStorage gl_RENDERBUFFER (getValue rb.format) rb.width rb.height
  insertResource $ LoadedRenderbuffer { renderbuffer: renderbuffer }

loadFramebuffer :: forall e r. Framebuffer -> RenderT e r ResourceId
loadFramebuffer (Framebuffer fb) = do
  fbo <- createFramebuffer
  bindFramebuffer gl_FRAMEBUFFER fbo
  case fb.color of
    (FBColorAttRenderTexture id) -> do
      texture :: LoadedTexture <- lookupResource id
      attachToFramebuffer texture gl_COLOR_ATTACHMENT0
    (FBColorAttRebderBuffer id) -> do
      renderbuffer :: LoadedRenderbuffer <- lookupResource id
      attachToFramebuffer renderbuffer gl_COLOR_ATTACHMENT0
  case fb.depth of
    Nothing -> pure unit
    Just id -> do
      renderbuffer :: LoadedRenderbuffer <- lookupResource id
      attachToFramebuffer renderbuffer gl_DEPTH_ATTACHMENT
  bindFramebuffer gl_FRAMEBUFFER glNullFramebufferObject
  insertResource $ LoadedFramebuffer { fbo: fbo 
                                     , color: fb.color
                                     , depth: fb.depth 
                                     }
