module PureGL.Mesh where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.State (get, modify)
import Data.Dynamic (toDynamic)
import Data.Lens (Lens', _Just, lens, over, set, view, (^.))
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Typeable (class Typeable, mkTyRep)
import PureGL.ECS (SystemWithComponents, Entity, _components, _ecsManager, _systemStates)
import PureGL.ECS.Component (hasComponent, insertComponent)
import PureGL.Renderer.Geometry (Geometry)
import PureGL.Renderer.RenderResource (loadResource)
import PureGL.Renderer.RenderState (ECSManagerRenderer, RenderT)
import PureGL.Renderer.Types (ResourceId)

data MeshError 
  = MeshNotFound Entity
  | MeshAlreadyExists Entity

instance typeableMeshError :: Typeable MeshError where
  typeOf _ = mkTyRep "PureGL.Mesh" "MeshError"

-- | Record for the `Mesh` component type.
type MeshRec = { geometry :: Geometry
               , geometryId :: ResourceId
               , loaded :: Boolean                    
               , visible :: Boolean
               }

-- | The `Mesh` is the component type for the mesh systesm.
newtype Mesh = Mesh MeshRec

-- | The system state object for the mesh system.
type MeshSystemState = SystemWithComponents Mesh ()

-- | The `ECSManager` type for the mesh system inclues a dependency on the
-- | renderer system.
type ECSManagerMesh r = ECSManagerRenderer ( mesh :: MeshSystemState | r )

-- | Create a `Mesh` from a given `Geometry`.
mkMesh :: Geometry -> Mesh 
mkMesh g = Mesh { geometry: g
                , geometryId: -1
                , loaded: false
                , visible: true
                }

-- | Add a `Mesh` to a given `Entity`.
addMesh :: ∀ e r. Entity -> Mesh -> RenderT ( mesh :: MeshSystemState | r) e Unit
addMesh e m = do
  has <- hasComponent (SProxy :: SProxy "mesh") e  
  case has of
    true -> throwError $ toDynamic $ MeshAlreadyExists e
    false -> insertComponent (SProxy :: SProxy "mesh") e m

-- | Load the `Geometry` of a `Mesh` from a given `Entity` into the renderer systesm.
loadMeshGeometry :: ∀ e r. Entity -> RenderT ( mesh :: MeshSystemState | r) e Unit
loadMeshGeometry e = do
  meshM <- getMesh e
  case meshM of
    Nothing -> throwError $ toDynamic $ MeshNotFound e
    Just mesh -> do
      id <- loadResource $ mesh ^. _mesh <<< _geometry
      modifyMesh e $ (set (_mesh <<< _loaded ) true) <<< (set (_mesh <<< _geometryId) id)

 -- LENSES and setters/getters
_meshSystem :: ∀ r.  Lens'{ mesh :: MeshSystemState | r } MeshSystemState
_meshSystem = prop (SProxy :: SProxy "mesh")

_mesh :: Lens' Mesh MeshRec
_mesh = lens (\(Mesh r) -> r) (\_ -> Mesh)

_geometry :: ∀ r. Lens' MeshRec Geometry
_geometry = prop (SProxy :: SProxy "geometry")

_geometryId :: ∀ r. Lens' MeshRec ResourceId
_geometryId = prop (SProxy :: SProxy "geometryId")

_loaded :: ∀ r. Lens' MeshRec Boolean
_loaded = prop (SProxy :: SProxy "loaded")

_visible :: ∀ r. Lens' MeshRec Boolean
_visible = prop (SProxy :: SProxy "visible")

getMesh :: ∀ r e. Entity -> RenderT ( mesh :: MeshSystemState| r) e (Maybe Mesh)
getMesh e = do
  state <- get
  pure $ (view $ _ecsManager <<< _systemStates <<< _meshSystem <<< _components <<< at e) state

modifyMesh :: ∀ r e. Entity -> (Mesh -> Mesh) -> RenderT ( mesh :: MeshSystemState| r) e Unit
modifyMesh e f = do
  state <- get
  modify $ over (_ecsManager <<< _systemStates <<< _meshSystem <<< _components <<< at e <<< _Just) f