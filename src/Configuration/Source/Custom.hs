module Configuration.Source.Custom
  ( toEnvParser,
  )
where

--------------------------------------------------------------------------------

import Barbies (ConstraintsB (..), TraversableB, btraverseC)
import Configuration.Setting (Custom (..), Setting (..), Source (..))
import Prelude (Applicative (..), Functor (..), IO, Maybe (..), Read, ($))

--------------------------------------------------------------------------------

toEnvParser :: (AllB Read b, TraversableB b, ConstraintsB b) => b Setting -> IO (b Maybe)
toEnvParser = btraverseC @Read $ \Setting {..} ->
  case optSource of
    Source _ _ _ (Just Custom {..}) -> fmap Just $ runCustom
    _ -> pure Nothing
