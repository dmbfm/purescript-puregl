module PureGL.Utils.Misc where

import Prelude

import Data.Array (tail)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | Executes two actions depending on a parameter of type `a`, returning | only
-- | the value from the last action.
connect :: forall a b m. Monad m => (a -> m b) -> (a -> m b) -> a -> m b
connect f g a = (f a) *> (g a) 

infixr 2 connect as >-

-- | Given a default value of type `b` and a `Maybe a`, | return a `Either b a`
eitherFromMaybe :: forall a b. b -> Maybe a -> Either b a
eitherFromMaybe l m = case m of
  Nothing -> Left l
  Just v -> Right v

-- | A tail function which returns an empty array | if the input is an empty
-- | array.
safeTail :: forall a. Array a -> Array a
safeTail xs = 
  case tail xs of
    Nothing -> []
    Just xs' -> xs'

-- | Merge two records
foreign import merge :: forall a b c. Union a b c => Record a -> Record b -> Record c