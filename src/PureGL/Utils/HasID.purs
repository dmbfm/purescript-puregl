module PureGL.Utils.HasID where

import Prelude

import Data.Array (head)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import PureGL.Utils.Misc (safeTail)

class HasID a where
  getIdCounter :: a -> Int
  getIdPool :: a -> Array Int
  modifyIdCounter :: (Int -> Int) -> a -> a
  modifyIdPool :: (Array Int -> Array Int) -> a  -> a

incrementIdCounter :: forall a. HasID a => a -> a
incrementIdCounter = modifyIdCounter (_ + 1)

resetIdCounter :: forall a. HasID a => a -> a
resetIdCounter = modifyIdCounter (\_ -> 0) 

addIdToPool :: forall a. HasID a => Int -> a -> a 
addIdToPool id = modifyIdPool (\ids -> [id] <> ids)

removeIdPoolHead :: forall a. HasID a => a -> a
removeIdPoolHead = modifyIdPool safeTail

clearIdPool :: forall a. HasID a => a -> a
clearIdPool = modifyIdPool (\_ -> [])

requestId :: forall a. HasID a => a -> (Tuple Int a)
requestId a = 
  case head (getIdPool a) of
    Nothing -> Tuple ((getIdCounter a) + 1) (incrementIdCounter a)
    Just id -> Tuple id (removeIdPoolHead a)

returnId :: forall a. HasID a => Int -> a -> a
returnId id = addIdToPool id
