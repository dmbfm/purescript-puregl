module PureGL.RenderResource where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (get, modify)
import Data.Map (Map, lookup)
import Data.Maybe (Maybe(..))
import PureGL.Framebuffer (LoadedFramebuffer, LoadedRenderbuffer)
import PureGL.Geometry (LoadedGeometry)
import PureGL.Program (LoadedProgram)
import PureGL.RenderState (RenderError(..), RenderState, RenderT, addLoadedFramebuffer, addLoadedGeometry, addLoadedProgram, addLoadedRenderbuffer, addLoadedTexture, genId)
import PureGL.Texture (LoadedTexture)
import PureGL.Types (ResourceId)
  
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

-- instance loadResourceGeometry :: LoadResource Geometry where
--   loadResource = loadGeometry    