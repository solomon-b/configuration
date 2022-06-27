{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

--------------------------------------------------------------------------------

import Barbies (AllBF, ApplicativeB (..), ConstraintsB, FunctorB, Rec (Rec), TraversableB, bzipWith)
import Configuration
import Control.Applicative (Alternative (..))
import Control.Lens
import Data.Aeson qualified as A
import Data.Aeson.Lens (AsNumber (_Number), AsValue (_String), key)
import Data.Text (Text)
import GHC.Generics (Generic, M1 (M1))
import Prelude

--------------------------------------------------------------------------------

data OptionsF f = OptionsF
  { _serverHostF :: f Text,
    _numThreadsF :: f Int,
    _verbosityF :: f Int
  }
  deriving stock (Generic)
  deriving anyclass (FunctorB, ApplicativeB, TraversableB, ConstraintsB)

--TODO(SOLOMON): Derive all this stuff implicitly
deriving stock instance (AllBF Show f OptionsF) => Show (OptionsF f)

deriving stock instance (AllBF Eq f OptionsF) => Eq (OptionsF f)

deriving anyclass instance (AllBF A.FromJSON f OptionsF) => A.FromJSON (OptionsF f)

instance (Alternative f) => Semigroup (OptionsF f) where
  (<>) = bzipWith (<|>)

instance (Alternative f) => Monoid (OptionsF f) where
  mempty = bpure empty

--------------------------------------------------------------------------------

-- | We define ALL our options in single term as 'OptionsF
-- Setting'. This term is then interpreted into the various source
-- "parsers."
options :: OptionsF Configuration.Setting
options =
  OptionsF
    { _serverHostF =
        Setting
          { optSource =
              Source
                (Just $ JSONFile $ findField $ key "host" . _String)
                (Just $ Env "SERVER_HOST")
                (Just $ Arg
                  { argCommand = Main,
                    argFlag = "host",
                    argMetaVar = "<HOST>",
                    argRequired = True
                  })
                Nothing,
            optDefault = Nothing,
            optDescription = "Host on which graphql-engine will listen"
          },
      _numThreadsF =
        Setting
          { optSource =
              Source
                (Just $ JSONFile $ findField $ key "num_threads" . _Number . to round)
                (Just $ Env "NUM_THREADS")
                (Just $ Arg
                  { argCommand = Main,
                    argFlag = "threads",
                    argMetaVar = "<THREADS>",
                    argRequired = False
                  })
                Nothing,
            optDefault = Nothing,
            optDescription = "Number of threads"
          },
      _verbosityF =
        Setting
          { optSource =
              Source
                (Just $ JSONFile $ findField $ key "verbosity" . _Number . to round)
                (Just $ Env "VERBOSITY")
                Nothing
                Nothing,
            optDefault = Just 1,
            optDescription = "Level of verbosity"
          }
    }
    
--------------------------------------------------------------------------------

-- | Fake JSON file for demo
readConfigFile :: IO A.Value
readConfigFile =
  pure $
    A.object
      [ "host" A..= A.String "example.com",
        "verbosity" A..= A.Null
      ]

getOptions :: IO (OptionsF Maybe)
getOptions = do
  argOptions <- toArgParser options
  envOptions <- toEnvParser options
  jsonOptions <- fmap (toJsonParser options) $ readConfigFile
  let defaultOptions = toDefaults options
  pure $ argOptions <> envOptions <> jsonOptions <> defaultOptions

main :: IO ()
main = getOptions >>= print
