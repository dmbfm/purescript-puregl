module Example.PureGL.Scene.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Array (insert)
import Data.Foldable (foldl)
import Data.List (List(..), (:))
import Data.Map (fromFoldable)
import Data.Tuple (Tuple(..))
import PureGL.Data.Tree (Tree, showTree, (|>))
import PureGL.Math.Matrix as M
import PureGL.Math.Quaternion (fromAxisRotation)
import PureGL.Math.Vector as V
import PureGL.Scene (SceneComponent, SceneState, updateScene)

sampleTree :: Tree Int
sampleTree = 
  0 |> 
      (1 |> 
          (3 |> Nil)
        : (4 |> Nil)
        : Nil
      ) 
    : (2 |> Nil) 
    : Nil

fTree :: Tree (Int -> Int)
fTree = ((+)1) |> ( ((+) 2) |> Nil) : Nil

sceneDataA :: SceneComponent
sceneDataA = { position: V.mkVector3 0.0 0.0 0.0
             , orientation: one
             , scale: V.mkVector3 1.0 1.0 1.0
             , transform: M.identity
             }

sceneDataB :: SceneComponent
sceneDataB = { position: V.mkVector3 1.0 2.0 3.0
             , orientation: fromAxisRotation 45.0 (V.mkVector3 0.0 1.0 0.0)
             , scale: V.mkVector3 1.0 1.0 1.0
             , transform: M.identity
             }

sceneState :: SceneState
sceneState = { components: fromFoldable [ Tuple 0 sceneDataA, Tuple 1 sceneDataB, Tuple 2 sceneDataA, Tuple 3 sceneDataB, Tuple 4 sceneDataA]
             , root: sampleTree
             }

printScene :: SceneState -> String
printScene s = show $ map (\comp -> (show comp.transform)) s.components

main = do
  log $ printScene sceneState
  log $ printScene (updateScene sceneState)
  --updateSceneM (\d -> log $ ":" <> (show d.transform)) sceneState


-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Console (CONSOLE, log)
-- import Control.Safely (traverse_)
-- import Data.Array (range)
-- import PureGL.Data.Tree (Node(..), foldTreeM_, mkNode)
-- import PureGL.ECS (ECSManager(..), execECSManagerT, fromSystemStates)
-- import PureGL.Scene (SceneState, sampleSceneState, updateScene, updateScene')
-- import PureGL.Utils.Log (logObject)

-- state :: ECSManager  { scene :: SceneState  }
-- state = fromSystemStates { scene: sampleSceneState }


-- tree :: Node Int
-- tree = 
--   mkNode 0 
--   [ mkNode 1 []
--   , mkNode 2 
--     [ mkNode 3 []
--     , mkNode 4 
--       [ mkNode 5 []]
--     ]
--   ]

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- main = do  
--   -- logObject "State" state  
--   -- state' <- execECSManagerT "Context" state $ do
--   --   updateScene
--   -- logObject "State'" state'
--   logObject "SSS" (updateScene' sampleSceneState)
--   pure unit

-- main = (flip $ (flip foldTreeM_) 0) tree $ \a b -> do
--   log $ (show a) <> ", " <> (show b)
--   pure $ b + 1
