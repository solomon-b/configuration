module Configuration.Source.Default
  ( toDefaults,
  )
where

--------------------------------------------------------------------------------

import Barbies (FunctorB (..))
import Configuration.Setting (Setting, optDefaultL)
import Control.Lens (preview, _Just)
import Prelude (Maybe, (.))

--------------------------------------------------------------------------------

toDefaults :: (FunctorB b) => b Setting -> b Maybe
toDefaults = bmap (preview (optDefaultL . _Just))
