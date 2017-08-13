module PureGL.Scene where

import Prelude
import Data.Tuple (Tuple(..))

newtype Node a = Node (Tuple a (Array (Node a)))

type SceneNodeData = {  }

type SceneNode = Node {}