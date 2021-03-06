module PureGL.Scene where

import Prelude

import Control.Comonad.Cofree (head, tail, (:<))
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Rec.Class (class MonadRec, Step(..), tailRec, tailRecM)
import Control.Monad.State (class MonadState, get, modify)
import Data.Lens (Lens', _Just, over, set, view)
import Data.Lens.At (at)
import Data.Lens.Record (prop)
import Data.List (List(..), (:))
import Data.Map (Map, empty, insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Tree (Tree)
import Data.Tree.Zipper (findDown, fromTree, insertChild, toTree)
import PureGL.ECS (ECSManager(..), Entity, SystemWithComponents, _components, _ecsManager, _systemStates)
import PureGL.ECS.Component (insertComponent)
import PureGL.Math.Matrix as M
import PureGL.Math.Quaternion (Quaternion(..))
import PureGL.Math.Vector as V

-- | The component for the Scene system. This is the data associated with a
-- | `Node` in the `SceneTree`.
type SceneComponent = { position :: V.Vector3
                      , orientation :: Quaternion
                      , scale :: V.Vector3
                      , transform :: M.Matrix4
                      }
-- | The `Tree` strcture of the Scene. Each `Node` holds an `Entity` ID.
type SceneTree = Tree Entity

-- | The ECS state for the Scene system. 
type SceneState =  SystemWithComponents SceneComponent ( root :: SceneTree )

-- | The `SceneState` for an empty scene.
emptySceneState :: SceneState
emptySceneState = { components: empty
                  , root: (-1) :< Nil 
                  }

-- | The root `Node` of a scene is identified by an special `Entity` ID of value (-1).
sceneRoot :: Entity
sceneRoot = -1

-- | Traverse a `SceneState`, updating the transformation matrices of all its
-- | `Node`s.
updateScene :: SceneState  -> SceneState
updateScene { root: root, components: cs } =
  { root: root, components: tailRec go { accum: M.identity :: M.Matrix4, nodes: (tail root), components: cs } }
  where
    go ::_ -> Step _ (Map Entity SceneComponent)
    go {accum: m, nodes: Nil, components: cs'} = Done cs'
    go {accum: m, nodes: n:ns, components: cs'} =     
      let key = (head n) in  
      case (lookup key cs') of
        Nothing -> Loop { accum: m, nodes: ns, components: cs'}
        Just nodeData -> 
          let m' =  M.translate nodeData.position $ M.rotate' nodeData.orientation $ M.scale nodeData.scale m
              nodeData' = set _transform m' nodeData 
          in Loop { accum: m
                  , nodes: ns
                  , components: insert key 
                                       nodeData' 
                                       (tailRec go { accum: m', nodes: (tail n), components: cs' }) 
                  }  

-- | Update the `SceneState` in a `ECSManager` `MonadState` (like a `ECSManagerT` stack.)
updateSceneSystem :: forall r m. MonadState (ECSManager { scene :: SceneState | r}) m =>
                                 m Unit
updateSceneSystem = 
  modify $ over (_ecsManager <<< _systemStates <<< _sceneState) updateScene
 
-- | Add a Scene Component to an Entity.
addSceneComponent :: forall r m. MonadState (ECSManager { scene :: SceneState | r}) m =>  
                                 Entity -> 
                                 Entity -> 
                                 SceneComponent -> 
                                 m Unit
addSceneComponent e p c = do
  insertComponent (SProxy :: SProxy "scene") e c
  modify $ over (_ecsManager <<< _systemStates <<< _sceneState <<< _root)
    \r -> case findDown p (fromTree r) of
            Nothing -> r
            Just loc -> toTree (insertChild (e :< Nil) loc)

-- LENSES
_sceneState :: forall r. Lens' { scene :: SceneState | r} SceneState
_sceneState = prop (SProxy :: SProxy "scene")

_transform :: Lens' SceneComponent M.Matrix4
_transform = prop (SProxy :: SProxy "transform")

_position :: Lens' SceneComponent V.Vector3
_position = prop (SProxy :: SProxy "position")

_orientation :: Lens' SceneComponent Quaternion
_orientation = prop (SProxy :: SProxy "orientation")

_scale :: Lens' SceneComponent V.Vector3
_scale = prop (SProxy :: SProxy "scale")

_root :: Lens' SceneState SceneTree
_root = prop (SProxy :: SProxy "root")

_sceneComponent :: Int -> Lens' SceneState (Maybe SceneComponent)
_sceneComponent id = _components <<< at id

-- Getters and Setters
setPosition :: Entity -> V.Vector3 -> SceneState -> SceneState
setPosition id p = set ((_sceneComponent id) <<< _Just <<< _position) p 
