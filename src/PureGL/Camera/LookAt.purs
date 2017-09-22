module PureGL.Camera.LookAt where

import Prelude

import Control.Monad.State (class MonadState, get)
import Data.Dynamic (fromDynamic)
import Data.Foldable (traverse_)
import Data.Lens (view, (.~))
import Data.Map (keys, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Typeable (class Typeable, mkTyRep, typeOf)
import Math (e)
import PureGL (PureGLT)
import PureGL.Camera (CameraComponent(..), _cameraSystem)
import PureGL.ECS (ECSManager(..), Entity, _components, _ecsManager, _systemStates)
import PureGL.ECS.Component (getComponent, modifyComponent)
import PureGL.Math.Matrix as M
import PureGL.Math.Quaternion as Q
import PureGL.Math.Vector as V
import PureGL.Scene (SceneComponent, SceneState, _orientation)

lookAt :: forall r m. MonadState (ECSManager { scene :: SceneState | r}) m =>
                      Entity -> 
                      V.Vector3 -> 
                      V.Vector3 -> 
                      m Unit
lookAt e point up = do
  scM <- getComponent (SProxy :: SProxy "scene") e
  case scM of
    Nothing -> pure unit
    Just sc -> do 
      let q = Q.lookAt sc.position point up
      modifyComponent (SProxy :: SProxy "scene") e ( _orientation .~  q )
