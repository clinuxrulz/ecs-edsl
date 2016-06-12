module EcsEdsl.Monad.MonadEntity where

import EcsEdsl.Types

class Monad m => MonadEntity m where
  setComponent :: Component -> m ()
  unsetComponent :: ComponentType -> m ()
  lookupComponent :: ComponentType -> m (TMaybe Component)
