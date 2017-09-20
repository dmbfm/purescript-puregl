module PureGL.Camera where

import Prelude

import Data.Dynamic (Dynamic(..))
import Data.Lens (Lens', lens)
import Data.Lens.Record (prop)
import Data.Map (empty)
import Data.Symbol (SProxy(..))
import PureGL.ECS (SystemWithComponents, Entity)
import PureGL.Math.Matrix as M

newtype CameraComponent = CameraComponent { viewMatrix :: M.Matrix4  
                                          , projectionMatrix :: M.Matrix4
                                          , cameraData :: Dynamic
                                          }

type CameraSystem = SystemWithComponents CameraComponent ( activeCamera :: Entity )

emptyCameraSystem :: CameraSystem
emptyCameraSystem = { components: empty
                    , activeCamera: -100
                    }

-- lenses
_cameraSystem :: forall r. Lens' { camera :: CameraSystem | r} CameraSystem
_cameraSystem = prop (SProxy :: SProxy "camera")

_CameraComponent :: Lens' CameraComponent _
_CameraComponent = lens (\( CameraComponent r ) -> r) (\_ -> CameraComponent)

_viewMatrix :: Lens' CameraComponent M.Matrix4
_viewMatrix = _CameraComponent <<< prop (SProxy :: SProxy "viewMatrix")

_projectionMatrix :: Lens' CameraComponent M.Matrix4
_projectionMatrix = _CameraComponent <<< prop (SProxy :: SProxy "projectionMatrix")