module PureGL.Renderer where

import Prelude

import Control.Bind ((=<<))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Class (get, modify)
import Control.Monad.State.Trans (StateT)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, head, mapWithIndex, tail, unsafeIndex)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Ord ((>))
import Data.Traversable (sequence)
import Partial.Unsafe (unsafePartial)
import PureGL.Context (Context(..), ContextR)
import PureGL.Framebuffer (class FBAttachable, FBColorAttachment(..), Framebuffer(..), LoadedFramebuffer(..), LoadedRenderbuffer(..), Renderbuffer(..), attachToFramebuffer)
import PureGL.Geometry (Geometry(..), LoadedGeometry(..), VertexAttribute(..))
import PureGL.Program (LoadedProgram(..), Program(..), buildProgram, getProgramAttribLocationsMap, getProgramUniformLocationsMap)
import PureGL.RenderState (RenderState, addIdPoolHead, addLoadedFramebuffer, addLoadedGeometry, addLoadedProgram, addLoadedRenderbuffer, addLoadedTexture, incrementIdCounter, removeIdPoolHead)
import PureGL.Texture (LoadedTexture(..), RenderTexture(..), Texture(..), TextureFormat(..), TexturePixels(..), TextureSampler(..))
import PureGL.Types (RenderError(..), ResourceId)
import PureGL.WebGL (bindBuffer, bindFramebuffer, bindRenderbuffer, bindTexture, bindVertexArray, bufferData, clear, clearColor, createBuffer, createFramebuffer, createRenderbuffer, createTexture, createVertexArray, drawArrays, enableVertexAttribArray, framebufferRenderbuffer, framebufferTexture2D, generateMipmap, getValue, renderbufferStorage, texImage2D, texImage2D', texParameterf, texParameteri, useProgram, vertexAttribPointer)
import PureGL.WebGL.Constants (gl_ARRAY_BUFFER, gl_COLOR_ATTACHMENT0, gl_COLOR_BUFFER_BIT, gl_DEPTH_ATTACHMENT, gl_FLOAT, gl_FRAMEBUFFER, gl_RENDERBUFFER, gl_STATIC_DRAW, gl_TEXTURE_MAG_FILTER, gl_TEXTURE_MAX_ANISOTROPY_EXT, gl_TEXTURE_MIN_FILTER, gl_TEXTURE_WRAP_S, gl_TEXTURE_WRAP_T, gl_TRIANGLES)
import PureGL.WebGL.Raw (nullBufferObject, nullFramebufferObject, nullVertexArrayObject)
import PureGL.WebGL.Raw as RAW
import PureGL.WebGL.Types (WebGLEff, WebGLTexture, GLenum)

-- | The `RenderStateT` is a Monad transformer that encapsualtes `WebGLEff` actions
-- | with error handling in a State transformer for `RenderState`.
-- type RenderStateT eff a = StateT RenderState (ExceptT RenderError (WebGLEff eff)) a 
type RenderStateT eff a = ExceptT RenderError (StateT RenderState (WebGLEff eff)) a

-- | Get a new ID from the `RenderState`.
genId :: forall eff. RenderStateT eff ResourceId
genId = do
  state <- get
  case (head state.idPool) of
    Just id -> do
      modify removeIdPoolHead
      pure id
    Nothing -> do
      modify incrementIdCounter
      pure state.idCounter

-- |  Release an id, returning it to the `idPool` of the 
-- | `RenderState` in the `RenderStateT`
returnId :: forall eff. Int ->  RenderStateT eff Unit
returnId id = do
  state <- get
  case (elem id state.idPool) of 
    true -> pure unit
    false -> do
        modify $ addIdPoolHead id
        pure unit

class InsertResource a where
  insertResource :: forall eff. a -> RenderStateT eff ResourceId
  lookupResource :: forall eff. ResourceId -> RenderStateT eff a

class LoadResource a where
  loadResource :: forall eff. a -> RenderStateT eff ResourceId

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

instance loadResourceGeometry :: LoadResource Geometry where
  loadResource = loadGeometry

_insertResource :: forall eff a. a -> (Int -> a -> RenderState -> RenderState) -> RenderStateT eff ResourceId
_insertResource r f = do
  id <- genId
  modify $ f id r
  pure id

_lookupResource :: forall eff a. ResourceId -> (RenderState -> Map ResourceId a) -> RenderStateT eff a 
_lookupResource id f = do
  state <- get
  case lookup id (f state) of
    Just r -> pure r
    Nothing -> throwError (LookupResourceError id)

loadGeometry :: forall eff.  Geometry -> RenderStateT eff ResourceId
loadGeometry (Geometry g) = do
  state <- get
  loadedGeo <- runReaderT (
    do 
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
      pure $ LoadedGeometry { buffer: buffer, vao: vao, vertexCount: g.vertexCount }
  ) state.context

  insertResource loadedGeo

loadProgram :: forall eff. Program -> RenderStateT eff (Either String ResourceId)
loadProgram (Program p) = do
  state <- get
  loadedProgramE <- liftEff $ runReaderT (
    do
      programE <- buildProgram p.vertexShaderSource p.fragmentShaderSource
      case programE of
        Left e -> pure $ Left e
        Right program -> do
          uniformLocsE <- getProgramUniformLocationsMap program p.uniforms
          case uniformLocsE of
            Left e' -> pure $ Left e'
            Right uniformLocs -> do
              attrLocsE <- getProgramAttribLocationsMap program p.attributes
              case attrLocsE of
                Left e'' -> pure $ Left e''
                Right attrLocs -> pure $ Right $ LoadedProgram { program: program 
                                                             , uniformLocations: uniformLocs
                                                             , attributeLocations: attrLocs
                                                             }
  ) state.context

  case loadedProgramE of
    Left e -> pure $ Left e
    Right loadedProgram -> do
      id <- insertResource loadedProgram
      pure $ Right id      

-- setSamplerState :: forall eff. WebGLTexture -> GLenum -> TextureSampler -> ContextR eff Unit
-- setSamplerState tex target (TextureSampler s) = do
--   (Context context) <- ask
--   texParameteri target gl_TEXTURE_MAG_FILTER (getValue s.magFilter)
--   texParameteri target gl_TEXTURE_MIN_FILTER (getValue s.minFilter)
--   texParameteri target gl_TEXTURE_WRAP_S (getValue s.wrapS)
--   texParameteri target gl_TEXTURE_WRAP_T (getValue s.wrapT)
--   case context.enabledExtensions.ext_texture_filter_anisotropic of
--     true -> texParameterf target gl_TEXTURE_MAX_ANISOTROPY_EXT  s.maxAnisotropy
--     false -> pure unit

-- loadTexture :: forall eff. Texture -> RenderStateT eff ResourceId
-- loadTexture tex@(Texture t) = do
--   state <- get
--   loadedTexture <- liftEff $ runReaderT doLoadTexture state.context
--   id <- addLoadedTextureT loadedTexture
--   pure id
--   where
--     doLoadTexture :: ContextR eff LoadedTexture
--     doLoadTexture = do
--       (Context context) <- ask
--       texture <- createTexture
--       let target = getValue t.textureTarget
--       bindTexture target texture      
--       let (TextureFormat fmt) = t.format
--       setSamplerState texture target t.sampler
--       case t.pixels of
--         (HTMLImagePixels p) -> texImage2D target 0 (getValue fmt.internalFormat) (getValue fmt.format) (getValue fmt.texelDataType) p
--         (HTMLVideoPixels p) -> texImage2D target 0 (getValue fmt.internalFormat) (getValue fmt.format) (getValue fmt.texelDataType) p
--         (HTMLCanvasPixels p) -> texImage2D target 0 (getValue fmt.internalFormat) (getValue fmt.format) (getValue fmt.texelDataType) p
--         (ImageDataPixels p) -> texImage2D target 0 (getValue fmt.internalFormat) (getValue fmt.format) (getValue fmt.texelDataType) p
--         (Uint8ArrayPixels p) -> texImage2D target 0 (getValue fmt.internalFormat) (getValue fmt.format) (getValue fmt.texelDataType) p
--       generateMipmap target
--       pure $ LoadedTexture { texture: texture, textureTarget: t.textureTarget }

-- loadRenderTexture :: forall eff. RenderTexture -> RenderStateT eff ResourceId
-- loadRenderTexture tex@(RenderTexture t) = do
--   state <- get
--   loadedTexture <- liftEff $ runReaderT doLoadTexture state.context
--   id <- addLoadedTextureT loadedTexture
--   pure id
--   where
--     doLoadTexture :: ContextR eff LoadedTexture
--     doLoadTexture = do
--       (Context context) <- ask
--       texture <- createTexture
--       let target = getValue (t.textureTarget)
--       bindTexture target texture      
--       let (TextureFormat fmt) = t.format
--       setSamplerState texture target t.sampler
--       texImage2D' target 0 (getValue fmt.internalFormat) t.size.width t.size.height (getValue fmt.format) (getValue fmt.texelDataType)
--       pure $ LoadedTexture { texture: texture, textureTarget: t.textureTarget }


-- loadRenderbuffer :: forall eff. Renderbuffer -> RenderStateT eff ResourceId
-- loadRenderbuffer (Renderbuffer rb) = do
--   state <- get
--   loadedRenderbuffer <- liftEff $ runReaderT doLoadRenderbuffer state.context
--   id <- addLoadedRenderbufferT loadedRenderbuffer
--   pure id
--   where
--     doLoadRenderbuffer :: ContextR eff LoadedRenderbuffer
--     doLoadRenderbuffer = do
--       renderbuffer <- createRenderbuffer
--       bindRenderbuffer gl_RENDERBUFFER renderbuffer
--       renderbufferStorage gl_RENDERBUFFER (getValue rb.format) rb.width rb.height
--       pure $ LoadedRenderbuffer { renderbuffer: renderbuffer }

-- loadFramebuffer :: forall eff. Framebuffer -> RenderStateT eff ResourceId
-- loadFramebuffer (Framebuffer fb) = do
--   state <- get
--   let gl = (\(Context ctx) -> ctx.glContext) $ state.context
--   fbo <- liftEff $ RAW.createFramebuffer gl
--   liftEff $ RAW.bindFramebuffer gl gl_FRAMEBUFFER fbo
--   case fb.color of 
--     (FBColorAttRenderTexture id) ->
--       case lookup id state.loadedTextures of
--         Nothing -> pure unit
--         Just tex -> liftEff $ runReaderT (attachToFramebuffer tex gl_COLOR_ATTACHMENT0) state.context
--     (FBColorAttRebderBuffer id) -> 
--       case lookup id state.loadedRenderbuffers of
--         Nothing -> pure unit
--         Just rb -> liftEff $ runReaderT (attachToFramebuffer rb gl_COLOR_ATTACHMENT0) state.context
--   case fb.depth of 
--     Nothing -> pure unit
--     Just id -> 
--       case lookup id state.loadedRenderbuffers of
--         Nothing -> pure unit
--         Just rb -> liftEff $ runReaderT (attachToFramebuffer rb gl_COLOR_ATTACHMENT0) state.context
--   liftEff $ RAW.bindFramebuffer gl gl_FRAMEBUFFER nullFramebufferObject
--   let loadedFramebuffer = LoadedFramebuffer { fbo: fbo, color: fb.color, depth: fb.depth }
--   id <- addLoadedFramebufferT loadedFramebuffer
--   pure id
  
-- renderGeometry :: forall eff. ResourceId -> ResourceId -> RenderStateT eff Unit
-- renderGeometry geoId progId = do
--   state <- get
--   case lookup geoId state.loadedGeometries of
--     Nothing -> pure unit
--     Just geo -> do
--       case lookup progId state.loadedPrograms of
--         Nothing -> pure unit
--         Just program -> liftEff $ runReaderT (doRenderGeometry geo program) state.context
  
--   where
--     doRenderGeometry :: LoadedGeometry -> LoadedProgram -> ContextR eff Unit
--     doRenderGeometry (LoadedGeometry g) (LoadedProgram p) = do
--       clearColor 0.0 0.0 0.0 1.0
--       clear gl_COLOR_BUFFER_BIT
--       useProgram p.program
--       bindVertexArray g.vao
--       drawArrays gl_TRIANGLES 0 g.vertexCount
