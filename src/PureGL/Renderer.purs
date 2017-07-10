module PureGL.Renderer where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.State.Class (get, modify)
import Control.Monad.State.Trans (StateT(..))
import Data.Array (cons, head, tail)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Map (Map, delete, insert, lookup)
import Data.Maybe (Maybe(..), fromJust)
import Data.Traversable (traverse)
import Partial.Unsafe (unsafePartial)
import PureGL.Buffer (Buffer(..), BufferResource(..), BufferTarget, BufferUsage, mkBuffer)
import PureGL.Context (Context)
import PureGL.Data.TypedArrays (Float32Array)
import PureGL.Geometry (Geometry(..), GeometryResource(..), mkGeometry)
import PureGL.Resource (loadResource)
import PureGL.VertexBuffer (Attribute(..), VertexBuffer(..), mkVertexBuffer)
import PureGL.WebGL (getValue)
import PureGL.WebGL as WGL
import PureGL.WebGL.Types (WebGLEff)

type RenderState = { context :: Context
                   , loadedBuffers :: Map Int BufferResource
                   , loadedGeometries :: Map Int GeometryResource
                   , idCounter :: Int
                   , idPool :: Array Int
                   }

type RenderStateT eff a = StateT RenderState (WebGLEff eff) a

-- | Increments the `RenderState`'s `idCounter` 
incrementIdCounter :: RenderState -> RenderState
incrementIdCounter s = s { idCounter = s.idCounter + 1}

-- | Remove the head element from the `RenderState`'s `idPool`
removeIdPoolHead :: RenderState -> RenderState
removeIdPoolHead s = case (tail s.idPool) of
  Just arr -> s { idPool = arr }
  Nothing -> s

-- | Add an id to the head of the `RenderState`'s `idPool`
addIdPoolHead :: Int -> RenderState -> RenderState
addIdPoolHead id s = s { idPool = cons id s.idPool }

-- | Generate a new id from in a `RenderStateT` monad 
-- | (get an id from the `idPool` if available, or increments the 
-- } `idCounter` otherwise).
genId :: forall eff. RenderStateT eff Int
genId = do
  state <- get
  case (head state.idPool) of
    Just id -> do
      modify removeIdPoolHead
      pure id
    Nothing -> do
      modify incrementIdCounter
      pure state.idCounter

-- | Release an id, returning it to the `idPool` of the 
-- | `RenderState` in the `RenderStateT`
returnId :: forall eff. Int ->  RenderStateT eff Unit
returnId id = do
  state <- get
  case (elem id state.idPool) of 
    true -> pure unit
    false -> do
        modify $ addIdPoolHead id
        pure unit

-- | Add a `BufferResource` to the `loadedBuffers` map of the `RenderState`
addLoadedBuffer :: Int -> BufferResource -> RenderState -> RenderState
addLoadedBuffer id b s = s { loadedBuffers = insert id b s.loadedBuffers }

getLoadedBuffer :: Int -> RenderState -> Maybe BufferResource 
getLoadedBuffer id s = lookup id s.loadedBuffers 

-- | Remove a `BufferResource` from the `loadedBuffers` map of the `RenderState`
removeLoadedBuffer :: Int -> RenderState -> RenderState
removeLoadedBuffer id s = s { loadedBuffers = delete id s.loadedBuffers }

-- | Add a `GeometryResource` to the `loadedBuffers` map of the `RenderState`
addLoadedGeometry :: Int -> GeometryResource -> RenderState -> RenderState
addLoadedGeometry id gr s = s { loadedGeometries = insert id gr s.loadedGeometries }

-- | Remove a `GeometryResource` from the `loadedBuffers` map of the `RenderState`
removeLoadedGeometry :: Int -> RenderState -> RenderState
removeLoadedGeometry id s = s { loadedGeometries = delete id s.loadedGeometries }

-- | Create a buffer from given parameters in the `RenderStateT` transformer
createBuffer :: forall eff. Float32Array -> BufferTarget -> BufferUsage -> RenderStateT eff Buffer
createBuffer src target usage = do
  state <- get
  id <- genId
  let buffer = mkBuffer src target usage id
  bufferResourceE <- liftEff $ runReaderT (loadResource buffer) state.context
  case (bufferResourceE) of
    Right bufferResource -> do 
        modify (addLoadedBuffer id bufferResource)
        pure buffer
    Left err -> pure buffer

-- | Delete a buffer in the `RenderStateT` transformer
deleteBuffer :: forall eff. Buffer -> RenderStateT eff Unit
deleteBuffer (Buffer b) = do
  state <- get
  case (lookup b.id state.loadedBuffers) of
    Just (BufferResource bufferResource) -> do
          liftEff $ runReaderT (WGL.deleteBuffer bufferResource.buffer) state.context
          returnId b.id
          pure unit
    Nothing -> pure unit

-- | Create a `VertexBuffer` 
createVertexBuffer :: forall eff.  Float32Array -> BufferTarget -> BufferUsage -> Array Attribute -> RenderStateT eff VertexBuffer
createVertexBuffer src target usage attrs = do
  buffer <- createBuffer src target usage
  pure $ mkVertexBuffer buffer attrs


  
