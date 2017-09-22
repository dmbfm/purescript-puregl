module PureGL.Renderer.RenderState where

import Prelude

import Control.Monad.State (class MonadState, get)
import Data.Lens (Lens', lens, over, view, (^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Map (Map, empty, insert)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Typeable (class Typeable, mkTyRep)
import PureGL.Context (Context)
import PureGL.ECS (ECSManager, ECSManagerT, _ecsManager, _systemStates)
import PureGL.Renderer.Framebuffer (LoadedFramebuffer, LoadedRenderbuffer)
import PureGL.Renderer.Geometry (LoadedGeometry)
import PureGL.Renderer.Program (LoadedProgram, ShaderType)
import PureGL.Renderer.Texture (LoadedTexture)
import PureGL.Renderer.Types (ResourceId)
import PureGL.Utils.HasID (class HasID)
import PureGL.WebGL.Types (WEBGL, WebGLEffRows)
import PureGL.Math.Vector as V

-- | Possible Renderer error types
data RenderError = 
    DefaultError 
  | LookupResourceError ResourceId
  | GetShaderParameterError
  | GetProgramParameterError
  | ShaderCompileError ShaderType String
  | ProgramLinkError String
  | UniformNotFound String
  | AttributeNotFount String
  | OtherErrors


-- | This type holds the state of the Renderer
type RenderStateRec = { loadedGeometries :: Map ResourceId LoadedGeometry                 
                      , loadedPrograms :: Map ResourceId LoadedProgram
                      , loadedTextures :: Map ResourceId LoadedTexture
                      , loadedFramebuffers :: Map ResourceId LoadedFramebuffer
                      , loadedRenderbuffers :: Map ResourceId LoadedRenderbuffer
                      , idCounter :: ResourceId
                      , idPool :: Array ResourceId
                      , clearColor :: V.Vector4
                      }

newtype RenderState = RenderState RenderStateRec

type ECSManagerRenderer r = ECSManager { renderer :: RenderState | r}

type RenderT r e a = ECSManagerT (WebGLEffRows e) Context { renderer :: RenderState | r} a

emptyRenderState :: RenderState 
emptyRenderState = RenderState
              { loadedGeometries: empty               
              , loadedPrograms: empty
              , loadedTextures: empty
              , loadedFramebuffers: empty
              , loadedRenderbuffers: empty
              , idCounter: 0
              , idPool: []
              , clearColor: V.mkVector4 0.0 0.0 0.0 1.0
              }

-- Instances
instance renderStateHasID :: HasID RenderState where
  getIdCounter = view $ _renderState <<< _idCounter
  getIdPool = view $ _renderState <<< _idPool
  modifyIdPool = over $ _renderState <<< _idPool
  modifyIdCounter = over $ _renderState <<< _idCounter

instance typeableRenderError :: Typeable RenderError where
  typeOf _ = mkTyRep "PureGL.Renderer.RenderState" "RenderError"

-- LENSES and getters/setters
_renderer :: forall r. Lens' { renderer :: RenderState | r } RenderState
_renderer =  prop (SProxy :: SProxy "renderer")

_renderState :: Lens' RenderState RenderStateRec
_renderState = lens (\(RenderState r) -> r) (\_ -> RenderState)

_loadedPrograms :: Lens' RenderStateRec (Map ResourceId LoadedProgram)
_loadedPrograms = prop (SProxy :: SProxy "loadedPrograms")

_loadedGeometries :: Lens' RenderStateRec (Map ResourceId LoadedGeometry)
_loadedGeometries = prop (SProxy :: SProxy "loadedGeometries")

_loadedTextures :: Lens' RenderStateRec (Map ResourceId LoadedTexture)
_loadedTextures = prop (SProxy :: SProxy "loadedTextures")

_loadedFramebuffers :: Lens' RenderStateRec (Map ResourceId LoadedFramebuffer)
_loadedFramebuffers = prop (SProxy :: SProxy "loadedFramebuffers")

_loadedRenderbuffers :: Lens' RenderStateRec (Map ResourceId LoadedRenderbuffer)
_loadedRenderbuffers = prop (SProxy :: SProxy "loadedRenderbuffers")

_idCounter :: Lens' RenderStateRec ResourceId
_idCounter = prop (SProxy :: SProxy "idCounter")

_idPool :: Lens' RenderStateRec (Array ResourceId)
_idPool = prop (SProxy :: SProxy "idPool")

_renderState' :: forall r. Lens' (ECSManagerRenderer r) RenderStateRec
_renderState' =  _ecsManager <<< _systemStates <<< _renderer <<< _renderState

getRenderer :: forall m r. Bind m => MonadState (ECSManagerRenderer r) m => m RenderState
getRenderer = do
  state <- get
  pure $ state ^. _renderer'

_renderer' :: forall r. Lens' (ECSManagerRenderer r) RenderState
_renderer' = _ecsManager <<< _systemStates <<< _renderer

getLoadedProgram :: ResourceId -> RenderState -> Maybe LoadedProgram
getLoadedProgram id = view $ _renderState <<< _loadedPrograms <<< (at id)

addLoadedProgram :: ResourceId -> LoadedProgram -> RenderState -> RenderState
addLoadedProgram id p = over (_renderState <<< _loadedPrograms) (insert id p)

getLoadedGeometry :: ResourceId -> RenderState -> Maybe LoadedGeometry
getLoadedGeometry id = view $ _renderState <<< _loadedGeometries <<< (at id)

addLoadedGeometry :: ResourceId -> LoadedGeometry -> RenderState -> RenderState
addLoadedGeometry id p = over (_renderState <<< _loadedGeometries) (insert id p)

getLoadedTexture :: ResourceId -> RenderState -> Maybe LoadedTexture
getLoadedTexture id = view $ _renderState <<< _loadedTextures <<< (at id)

addLoadedTexture :: ResourceId -> LoadedTexture -> RenderState -> RenderState
addLoadedTexture id p = over (_renderState <<< _loadedTextures) (insert id p)

getLoadedFramebuffer :: ResourceId -> RenderState -> Maybe LoadedFramebuffer
getLoadedFramebuffer id = view $ _renderState <<< _loadedFramebuffers <<< (at id)

addLoadedFramebuffer :: ResourceId -> LoadedFramebuffer -> RenderState -> RenderState
addLoadedFramebuffer id p = over (_renderState <<< _loadedFramebuffers) (insert id p)

getLoadedRenderbuffer :: ResourceId -> RenderState -> Maybe LoadedRenderbuffer
getLoadedRenderbuffer id = view $ _renderState <<< _loadedRenderbuffers <<< (at id)

addLoadedRenderbuffer :: ResourceId -> LoadedRenderbuffer -> RenderState -> RenderState
addLoadedRenderbuffer id p = over (_renderState <<< _loadedRenderbuffers) (insert id p)