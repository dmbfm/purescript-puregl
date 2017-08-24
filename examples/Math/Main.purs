module PureGL.Examples.Math.Main where

import Prelude

import Control.Monad.Eff.Console (CONSOLE, log)
import PureGL.Math.Vector.Fast as FV
import PureGL.Math.Matrix.Fast as FM
import PureGL.Math.Matrix as M
import PureGL.Math.Vector as V
import PureGL.WebGL.Types (WebGLEff)

foreign import runTests :: forall e a. a ->  WebGLEff e Unit

foreign import vArrAdd :: forall a. a -> a -> a 

vArrAdd2 a b = vArrAdd a b

main :: forall e. WebGLEff ( console :: CONSOLE | e) Unit
main = runTests { addFV: addFV, addV: addV, mulFM3: mulFM3, mulM3: mulM3, vArrAdd: vArrAdd2 }

addFV :: forall e. FV.FVector4 -> FV.FVector4 -> WebGLEff e Unit
addFV a b = FV.add a b b

addV :: V.Vector4 -> V.Vector4 ->V.Vector4
addV v1 v2 = V.add v1 v2


mulFM3 :: forall e. FM.FMatrix3 -> FM.FMatrix3 -> FM.FMatrix3 -> WebGLEff e Unit
mulFM3 a b c = FM.mulMatrix a b c 

mulM3 :: M.Matrix3 -> M.Matrix3 -> M.Matrix3
mulM3 a b = M.mulMatrix a b