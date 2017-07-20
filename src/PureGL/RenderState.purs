module PureGL.RenderState where

import Prelude

import Data.Array (cons, tail)
import Data.Map (Map, delete, insert, lookup)
import Data.Maybe (Maybe(..))
import PureGL.Context (Context(..))
import PureGL.Framebuffer (LoadedFramebuffer(..), LoadedRenderbuffer(..))
import PureGL.Geometry (LoadedGeometry(..))
import PureGL.Program (LoadedProgram(..))
import PureGL.Texture (LoadedTexture(..))
import PureGL.Types (ResourceId)

-- | This type holds the state of the Renderer
type RenderState =  { context :: Context 
                    , loadedGeometries :: Map ResourceId LoadedGeometry
                    , loadedPrograms :: Map ResourceId LoadedProgram
                    , loadedTextures :: Map ResourceId LoadedTexture
                    , loadedFramebuffers :: Map ResourceId LoadedFramebuffer
                    , loadedRenderbuffers :: Map ResourceId LoadedRenderbuffer
                    , idCounter :: ResourceId
                    , idPool :: Array ResourceId
                    }

-- | Increments the `RenderState`'s `idCounter` 
incrementIdCounter :: RenderState -> RenderState
incrementIdCounter s =  s { idCounter = s.idCounter + 1}

-- | Remove the head element from the `RenderState`'s `idPool`
removeIdPoolHead :: RenderState -> RenderState
removeIdPoolHead s = case (tail s.idPool) of
  Just arr -> s { idPool = arr }
  Nothing -> s

-- | Add an id to the head of the `RenderState`'s `idPool`
addIdPoolHead :: Int -> RenderState -> RenderState
addIdPoolHead id s = s { idPool = cons id s.idPool }

-- | Add a `LoadedGeometry` Resource to a `RenderState`
addLoadedGeometry :: Int -> LoadedGeometry -> RenderState ->  RenderState
addLoadedGeometry id lg s = s { loadedGeometries = insert id lg s.loadedGeometries }

-- | Add a `LoadedProgram` Resource to a `RenderState`
addLoadedProgram :: Int -> LoadedProgram -> RenderState -> RenderState
addLoadedProgram id lp s = s { loadedPrograms = insert id lp s.loadedPrograms }

-- | Add a `LoadedTexture` Resource to a `RenderState`
addLoadedTexture :: Int -> LoadedTexture -> RenderState -> RenderState
addLoadedTexture id lt s = s { loadedTextures = insert id lt s.loadedTextures }

-- | Add a `LoadedFramebuffer` Resource to a `RenderState`
addLoadedFramebuffer :: Int -> LoadedFramebuffer -> RenderState -> RenderState
addLoadedFramebuffer id lt s = s { loadedFramebuffers = insert id lt s.loadedFramebuffers }

-- | Add a `LoadedRenderbuffer` Resource to a `RenderState`
addLoadedRenderbuffer :: Int -> LoadedRenderbuffer -> RenderState -> RenderState
addLoadedRenderbuffer id lt s = s { loadedRenderbuffers = insert id lt s.loadedRenderbuffers }

-- | Attempts to search a `LoadedGeometry` in `RenderState` by its ID
lookupLoadedGeometry :: Int -> RenderState -> Maybe LoadedGeometry
lookupLoadedGeometry id state = lookup id state.loadedGeometries

-- | Attempts to search a `LoadedTexture` in `RenderState` by its ID
lookupLoadedTexture :: Int -> RenderState -> Maybe LoadedTexture
lookupLoadedTexture id state = lookup id state.loadedTextures

-- | Attempts to search a `LoadedProgram` in `RenderState` by its ID
lookupLoadedProgram :: Int -> RenderState -> Maybe LoadedProgram
lookupLoadedProgram id state = lookup id state.loadedPrograms

-- | Attempts to search a `LoadedRenderbuffer` in `RenderState` by its ID
lookupLoadedRenderbuffer :: Int -> RenderState -> Maybe LoadedRenderbuffer
lookupLoadedRenderbuffer id state = lookup id state.loadedRenderbuffers

-- | Attempts to search a `LoadedFramebuffer` in `RenderState` by its ID
lookupLoadedFramebuffer :: Int -> RenderState -> Maybe LoadedFramebuffer
lookupLoadedFramebuffer id state = lookup id state.loadedFramebuffers

-- | Deletes a `LoadedGeometry` of a given ID from the `RenderState`
deleteLoadedGeometry :: Int -> RenderState -> RenderState 
deleteLoadedGeometry id s = s { loadedGeometries = delete id s.loadedGeometries }

-- | Deletes a `LoadedTexture` of a given ID from the `RenderState`
deleteLoadedTexture :: Int -> RenderState -> RenderState 
deleteLoadedTexture id s = s { loadedTextures = delete id s.loadedTextures }

-- | Deletes a `LoadedProgram` of a given ID from the `RenderState`
deleteLoadedProgram :: Int -> RenderState -> RenderState 
deleteLoadedProgram id s = s { loadedPrograms = delete id s.loadedPrograms }

-- | Deletes a `LoadedRenderbuffer` of a given ID from the `RenderState`
deleteLoadedRenderbuffer :: Int -> RenderState -> RenderState 
deleteLoadedRenderbuffer id s = s { loadedRenderbuffers = delete id s.loadedRenderbuffers }

-- | Deletes a `LoadedFramebuffer` of a given ID from the `RenderState`
deleteLoadedFramebuffer :: Int -> RenderState -> RenderState 
deleteLoadedFramebuffer id s = s { loadedFramebuffers = delete id s.loadedFramebuffers }
