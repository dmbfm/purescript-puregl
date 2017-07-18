module PureGL.Utils.Misc where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

eitherFromMaybe :: forall a b. b -> Maybe a -> Either b a
eitherFromMaybe l m = case m of
  Nothing -> Left l
  Just v -> Right v
