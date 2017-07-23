module PureGL.RenderResource where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (get, modify)
import Data.Array (mapWithIndex, unsafeIndex)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial)
import PureGL.Framebuffer (FBColorAttachment(..), Framebuffer(..), LoadedFramebuffer(..), LoadedRenderbuffer(..), Renderbuffer(..))
import PureGL.GLConstant (getValue)
import PureGL.Geometry (Geometry(..), LoadedGeometry(..), VertexAttribute(..))
import PureGL.Internal.Framebuffer (attachToFramebuffer)
import PureGL.Internal.Program (buildProgram, getProgramAttribLocationsMap, getProgramUniformLocationsMap, setUniformsMap)
import PureGL.Internal.Texture (setSamplerState)
import PureGL.Program (LoadedProgram(..), Program(..))
import PureGL.RenderState (RenderError(..), RenderState, RenderT, addLoadedFramebuffer, addLoadedGeometry, addLoadedProgram, addLoadedRenderbuffer, addLoadedTexture, genId)
import PureGL.Texture (LoadedTexture(..), RenderTexture(..), Texture(..), TextureFormat(..), TexturePixels(..))
import PureGL.Types (ResourceId)
import PureGL.WebGL (bindBuffer, bindFramebuffer, bindRenderbuffer, bindTexture, bindVertexArray, bufferData, createBuffer, createFramebuffer, createRenderbuffer, createTexture, createVertexArray, enableVertexAttribArray, generateMipmap, renderbufferStorage, texImage2D, texImage2D', useProgram, vertexAttribPointer)
import PureGL.WebGL.Constants (gl_ARRAY_BUFFER, gl_COLOR_ATTACHMENT0, gl_DEPTH_ATTACHMENT, gl_FLOAT, gl_FRAMEBUFFER, gl_RENDERBUFFER, gl_STATIC_DRAW)
import PureGL.WebGL.Raw (nullBufferObject, nullFramebufferObject, nullVertexArrayObject)
  
class InsertResource a where
  insertResource :: forall eff. a -> RenderT eff ResourceId
  lookupResource :: forall eff. ResourceId -> RenderT eff a

class LoadResource a where
  loadResource :: forall eff. a -> RenderT eff ResourceId

instance loadedGeometryInsertResource :: InsertResource LoadedGeometry where
  insertResource r = _insertResource r addLoadedGeometry
  lookupResource id = _lookupResource id (\s -> s.loadedGeometries)

instance insertResourceLoadedTexture :: InsertResource LoadedTexture where
  insertResource r = _insertResource r addLoadedTexture
  lookupResource id = _lookupResource id (\s -> s.loadedTextures)

instance insertResourceLoadedRenderbuffer :: InsertResource LoadedRenderbuffer where
  insertResource r = _insertResource r addLoadedRenderbuffer
  lookupResource id = _lookupResource id (\s -> s.loadedRenderbuffers)

instance insertResourceLoadedProgram :: InsertResource LoadedProgram where
  insertResource r = _insertResource r addLoadedProgram
  lookupResource id = _lookupResource id (\s -> s.loadedPrograms)

instance insertResourceLoadedFramebuffer :: InsertResource LoadedFramebuffer where
  insertResource r = _insertResource r addLoadedFramebuffer
  lookupResource id = _lookupResource id (\s -> s.loadedFramebuffers)

_insertResource :: forall eff a. a -> (Int -> a -> RenderState -> RenderState) -> RenderT eff ResourceId
_insertResource r f = do
  id <- genId
  modify $ f id r
  pure id

_lookupResource :: forall eff a. ResourceId -> (RenderState -> Map ResourceId a) -> RenderT eff a 
_lookupResource id f = do
  state <- get
  case lookup id (f state) of
    Just r -> pure r
    Nothing -> throwError (LookupResourceError id)

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

loadGeometry :: forall eff.  Geometry -> RenderT eff ResourceId
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
  bindVertexArray nullVertexArrayObject
  bindBuffer gl_ARRAY_BUFFER nullBufferObject
  insertResource $  LoadedGeometry { buffer: buffer, vao: vao, vertexCount: g.vertexCount }

loadProgram :: forall eff. Program -> RenderT eff ResourceId
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

loadTexture :: forall eff. Texture -> RenderT eff ResourceId
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

loadRenderTexture :: forall eff. RenderTexture -> RenderT eff ResourceId
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

loadRenderbuffer :: forall eff. Renderbuffer -> RenderT eff ResourceId
loadRenderbuffer (Renderbuffer rb) = do
  renderbuffer <- createRenderbuffer
  bindRenderbuffer gl_RENDERBUFFER renderbuffer
  renderbufferStorage gl_RENDERBUFFER (getValue rb.format) rb.width rb.height
  insertResource $ LoadedRenderbuffer { renderbuffer: renderbuffer }

loadFramebuffer :: forall eff. Framebuffer -> RenderT eff ResourceId
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
  bindFramebuffer gl_FRAMEBUFFER nullFramebufferObject
  insertResource $ LoadedFramebuffer { fbo: fbo 
                                     , color: fb.color
                                     , depth: fb.depth 
                                     }
