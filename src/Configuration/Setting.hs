module Configuration.Setting where

--------------------------------------------------------------------------------

import Control.Lens (Lens', preview, (<&>), _Just)
import Data.Aeson qualified as A
import Data.Coerce (coerce)
import Data.Functor.Compose (Compose (..))
import Data.Text (Text)
import Prelude (Bool, Maybe, (.))

--------------------------------------------------------------------------------

-- | An Option sourced from Argv
data Arg = Arg
  { argCommand :: Command,
    argFlag :: Text,
    argMetaVar :: Text,
    argRequired :: Bool
  }

-- | Opt Parse Applicative command choice.
data Command = Clean | Export | Downgrade | Main | Serve | Version

-- | An Option sourced from a JSON file
newtype JSONFile a = JSONFile {jsonField :: A.Value -> Maybe a}

jsonFieldL :: Lens' (JSONFile a) (Compose ((->) A.Value) Maybe a)
jsonFieldL f (JSONFile field) = f (Compose field) <&> coerce

-- | An Option sourced from the Environment
newtype Env = Env {envVar :: Text}

envVarL :: Lens' Env Text
envVarL f (Env var) = f var <&> Env

--------------------------------------------------------------------------------

data Source a = Source
  { srcJson :: Maybe (JSONFile a),
    srcEnv :: Maybe Env,
    srcArg :: Maybe Arg
  }

srcJsonL :: Lens' (Source a) (Maybe (JSONFile a))
srcJsonL f src@Source {..} =
  f srcJson <&> \srcJson' -> src {srcJson = srcJson'}

srcEnvL :: Lens' (Source a) (Maybe Env)
srcEnvL f src@Source {..} =
  f srcEnv <&> \srcEnv' -> src {srcEnv = srcEnv'}

srcArgL :: Lens' (Source a) (Maybe Arg)
srcArgL f src@Source {..} =
  f srcArg <&> \srcArg' -> src {srcArg = srcArg'}

--------------------------------------------------------------------------------

-- |'Setting a' contains all the information required to produce an
-- @a@.
-- 
-- - Required or Optional
-- - Default Value
-- - Source (args, env, lux, custom io action? launchdarkly?)
-- - Documentation
data Setting a = Setting
  { optSource :: Source a,
    optDefault :: Maybe a,
    optDescription :: Text
  }

optSourceL :: Lens' (Setting a) (Source a)
optSourceL f opts@Setting {optSource} =
  f optSource <&> \optSource' -> opts {optSource = optSource'}

optDefaultL :: Lens' (Setting a) (Maybe a)
optDefaultL f opts@Setting {optDefault} =
  f optDefault <&> \optDefault' -> opts {optDefault = optDefault'}

jsonSourceP :: Setting a -> Maybe (Compose ((->) A.Value) Maybe a)
jsonSourceP settings = preview (optSourceL . srcJsonL . _Just . jsonFieldL) settings
