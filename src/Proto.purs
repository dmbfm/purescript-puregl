module Proto where

-- import Prelude

-- import Control.Monad.Eff (Eff)
-- import Control.Monad.Eff.Class (class MonadEff, liftEff)
-- import Control.Monad.Eff.Console (CONSOLE, log)
-- import Control.Monad.Error.Class (class MonadThrow, catchError, throwError, try)
-- import Control.Monad.Except (ExceptT(..))
-- import Control.Monad.State (StateT(..), get)
-- import Data.Dynamic (Dynamic(..), toDynamic)
-- import Data.Lens ((^.))
-- import Data.Lens.Record (prop)
-- import Data.Symbol (SProxy(..))
-- import Data.Typeable (class Typeable, TypeRep(..), mkTyRep, typeOf)
-- import PureGL.Utils.Misc ((>-))
-- import Type.Proxy (Proxy(..))
-- import Unsafe.Coerce (unsafeCoerce)

-- data MyError 
--   = DefaultError
--   | OtherError


-- instance typeableMyError :: Typeable MyError where
--   typeOf _ = mkTyRep "Proto" "MyError"

-- myErrorTR :: TypeRep
-- myErrorTR = typeOf (Proxy :: Proxy MyError)

-- intTR :: TypeRep
-- intTR = typeOf (Proxy :: Proxy Int)

-- getType :: forall a. Typeable a => a -> TypeRep
-- getType _ = typeOf (Proxy :: Proxy a)

-- type M e eff a = ExceptT Dynamic (Eff (console :: CONSOLE | eff)) a

-- handleError :: forall e m. MonadEff (console :: CONSOLE | e) m => MonadThrow Dynamic m => Dynamic -> m Unit
-- handleError (Dynamic t v) 
--   | t == myErrorTR = 
--     case (unsafeCoerce v) of 
--       DefaultError -> do
--         liftEff $ log "DefaultError"
--         pure unit
--       OtherError -> pure unit
--   | t == intTR = pure unit
--   | otherwise = pure unit

-- m :: forall e eff. M e eff Unit
-- m = do
--   catchError (throwError (toDynamic DefaultError)) (handleError >- handleError)
--   pure unit

