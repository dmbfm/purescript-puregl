module PureGL.Material where

import Prelude
import Data.Lens (Lens', lens)
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import PureGL.ECS (SystemWithComponents)
import PureGL.Renderer.Program (Program(..))
import PureGL.Renderer.Types (ResourceId)

type MaterialComponentRec = { program :: Program
                            , programId :: ResourceId 
                            , loaded :: Boolean
                            }

newtype MaterialComponent = MaterialComponent MaterialComponentRec

type MaterialSystem = SystemWithComponents MaterialComponent ()

-- lenses
_materialSystem :: forall r. Lens' { material :: MaterialSystem | r} MaterialSystem
_materialSystem = prop (SProxy :: SProxy "material")

_materialComponent :: Lens' MaterialComponent MaterialComponentRec
_materialComponent = lens (\(MaterialComponent r) -> r) (\_ -> MaterialComponent)

_program :: Lens' MaterialComponent Program
_program  = _materialComponent <<<  prop (SProxy :: SProxy "program")

_programId :: Lens' MaterialComponent ResourceId
_programId  = _materialComponent <<<  prop (SProxy :: SProxy "programId")

_loaded :: Lens' MaterialComponent Boolean
_loaded  = _materialComponent <<<  prop (SProxy :: SProxy "loaded")