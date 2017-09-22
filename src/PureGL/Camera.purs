module PureGL.Camera where

import Prelude

import Control.Monad.State (class MonadState, get, modify)
import Data.Dynamic (Dynamic(..))
import Data.Foldable (traverse_)
import Data.Lens (Lens', lens, (.~), (^.))
import Data.Lens.Record (prop)
import Data.Map (empty, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import PureGL.ECS (ECSManager(..), Entity, SystemWithComponents, _components, _ecsManager, _systemStates)
import PureGL.ECS.Component (getComponent, modifyComponent)
import PureGL.Math.Matrix (mkPerspective'', mkRotation', translate)
import PureGL.Math.Matrix as M
import PureGL.Math.Vector as V
import PureGL.Scene (SceneState)

newtype CameraComponent = CameraComponent { viewMatrix :: M.Matrix4  
                                          , projectionMatrix :: M.Matrix4
                                          , aspect :: Number
                                          , fov :: Number
                                          , near :: Number
                                          , far :: Number
                                          , updatable :: Boolean
                                          }

mkCamera :: Number -> Number -> CameraComponent
mkCamera aspect fov = mkCamera' aspect fov true 

mkCamera' :: Number -> Number -> Boolean -> CameraComponent
mkCamera'  aspect fov u = mkCamera'' aspect fov 0.1 1000.0 u

mkCamera'' :: Number -> Number -> Number -> Number -> Boolean -> CameraComponent
mkCamera'' aspect fov near far u = CameraComponent { viewMatrix: M.identity
                                          , projectionMatrix: M.identity
                                          , aspect: aspect
                                          , fov: fov
                                          , near: near
                                          , far: far
                                          , updatable: u
                                          }

type CameraSystem = SystemWithComponents CameraComponent ( activeCamera :: Entity )

emptyCameraSystem :: CameraSystem
emptyCameraSystem = { components: empty
                    , activeCamera: -100
                    }

-- | Go trough all components of the camera system and update their view and projection matrices
-- | based on their scene component's transform matrix.
updateCameraSystem :: forall m r.  MonadState (ECSManager { camera :: CameraSystem, scene :: SceneState  | r}) m => m Unit
updateCameraSystem = do
  state <- get
  let cameraComponents :: Array (Tuple Entity CameraComponent) 
      cameraComponents = toUnfoldable $ state ^. (_ecsManager <<< _systemStates <<< _cameraSystem <<< _components)
  traverse_ updateCamera cameraComponents

  where
    updateCamera (Tuple e cc) = 
      case cc ^. _updatable of
        false -> pure unit
        true -> do
          scM <- getComponent (SProxy :: SProxy "scene") e
          case scM of
            Nothing -> pure unit
            Just sc -> do              
              --let viewMatrix = translate (V.mul (-1.0) sc.position) (mkRotation' (recip sc.orientation))
              let viewMatrix = case M.invert sc.transform of
                                Nothing -> M.identity
                                Just m -> m
              let projectionMatrix = mkPerspective''  (cc ^. _aspect) (cc ^. _fov) (cc ^. _near) (cc ^. _far)
              modifyComponent (SProxy :: SProxy "camera") e ((_viewMatrix .~ viewMatrix) <<< (_projectionMatrix .~ projectionMatrix))
              pure unit



-- lenses
_cameraSystem :: forall r. Lens' { camera :: CameraSystem | r} CameraSystem
_cameraSystem = prop (SProxy :: SProxy "camera")

_CameraComponent :: Lens' CameraComponent _
_CameraComponent = lens (\( CameraComponent r ) -> r) (\_ -> CameraComponent)

_viewMatrix :: Lens' CameraComponent M.Matrix4
_viewMatrix = _CameraComponent <<< prop (SProxy :: SProxy "viewMatrix")

_projectionMatrix :: Lens' CameraComponent M.Matrix4
_projectionMatrix = _CameraComponent <<< prop (SProxy :: SProxy "projectionMatrix")

_updatable :: Lens' CameraComponent Boolean
_updatable = _CameraComponent <<< prop (SProxy :: SProxy "updatable")

_aspect :: Lens' CameraComponent Number
_aspect = _CameraComponent <<< prop (SProxy :: SProxy "aspect")

_fov :: Lens' CameraComponent Number
_fov = _CameraComponent <<< prop (SProxy :: SProxy "fov")

_near :: Lens' CameraComponent Number
_near = _CameraComponent <<< prop (SProxy :: SProxy "near")

_far :: Lens' CameraComponent Number
_far = _CameraComponent <<< prop (SProxy :: SProxy "far")