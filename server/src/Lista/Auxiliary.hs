module Lista.Auxiliary (module Lista.Auxiliary) where

import           Control.Monad.Except (MonadError, liftEither)
import           Relude


maybeToRightM :: MonadError l m => l -> Maybe r -> m r
maybeToRightM err = liftEither . maybeToRight err

maybeToLeftM_ :: MonadError l m => Maybe l -> m ()
maybeToLeftM_ = liftEither . maybeToLeft ()
