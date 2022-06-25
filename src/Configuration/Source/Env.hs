module Configuration.Source.Env
  ( toEnvParser,
  )
where

--------------------------------------------------------------------------------

import Barbies (ConstraintsB (..), TraversableB, btraverseC)
import Configuration.Setting (Env (..), Setting (..), Source (..))
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Prelude (Applicative (..), IO, Maybe (..), Monad (..), Read, ($), (.))

--------------------------------------------------------------------------------

toEnvParser :: (AllB Read b, TraversableB b, ConstraintsB b) => b Setting -> IO (b Maybe)
toEnvParser = btraverseC @Read $ \Setting {..} ->
  case optSource of
    Source _ (Just (Env var)) _ -> readEnv var
    _ -> pure Nothing

readEnv :: Read a => Text -> IO (Maybe a)
readEnv envKey = do
  lookupEnv (T.unpack envKey)
    >>= pure . \case
      Just x -> readMaybe x
      Nothing -> Nothing
