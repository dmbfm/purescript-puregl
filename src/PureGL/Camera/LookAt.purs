module PureGL.Camera.LookAt where

import Prelude

import Control.Monad.State (get)
import Data.Dynamic (fromDynamic)
import Data.Foldable (traverse_)
import Data.Lens (view)
import Data.Map (keys, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, mkTyRep, typeOf)
import Math (e)
import PureGL (PureGLT)
import PureGL.Camera (CameraComponent(..), _cameraSystem)
import PureGL.ECS (Entity, _components, _ecsManager, _systemStates)
import PureGL.ECS.Component (getComponent, modifyComponent)
import PureGL.Math.Matrix as M
import PureGL.Math.Vector as V
import PureGL.Scene (SceneComponent)

newtype LookAtCamera = LookAtCamera { point :: V.Vector3 
                                    , up :: V.Vector3
                                    , aspect :: Number
                                    , fov :: Number
                                    , near :: Number
                                    , far :: Number
                                    }


instance typeableLookAtCamera :: Typeable LookAtCamera where
  typeOf _ = mkTyRep "PureGL.Camera" "LookAtCamera"

updateCamera :: SceneComponent -> CameraComponent -> CameraComponent
updateCamera sc cc@(CameraComponent c) = 
  case (fromDynamic c.cameraData) of
    Nothing -> cc
    Just (LookAtCamera lac) -> 
      CameraComponent { viewMatrix: M.mkLookAt sc.position lac.point lac.up  
                      , projectionMatrix: M.mkPerspective'' lac.aspect lac.fov lac.near lac.far
                      , cameraData: c.cameraData
                      }

updateLookAtCameras :: forall r e. PureGLT r e Unit
updateLookAtCameras = do
  state <- get
  let cs = view (_ecsManager <<< _systemStates <<< _cameraSystem <<< _components) state
  traverse_ updt (keys cs)
  where 
    updt e= do
      scM <- getComponent (SProxy :: SProxy "scene") e
      case scM of
        Nothing -> pure unit
        Just sc -> modifyComponent (SProxy :: SProxy "camera") e (updateCamera sc) 
