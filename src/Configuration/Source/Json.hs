{-# LANGUAGE RankNTypes #-}

module Configuration.Source.Json
  ( toJsonParser,
    findField,
  )
where

--------------------------------------------------------------------------------

import Barbies (FunctorB (..), TraversableB, bsequence)
import Configuration.Setting (Setting, jsonFieldL, optSourceL, srcJsonL)
import Control.Lens (Fold, preview, _Just)
import Data.Aeson qualified as A
import Data.Functor.Compose (Compose (..))
import Data.Maybe (fromMaybe)
import Prelude (Maybe (..), const, ($), (.))

--------------------------------------------------------------------------------

toJsonParser' :: (FunctorB b) => b Setting -> b (Compose ((->) A.Value) Maybe)
toJsonParser' =
  bmap (fromMaybe (Compose $ const Nothing) . preview (optSourceL . srcJsonL . _Just . jsonFieldL))

toJsonParser :: (TraversableB b) => b Setting -> A.Value -> b Maybe
toJsonParser options = bsequence (toJsonParser' options)

findField :: Fold A.Value a -> A.Value -> Maybe a
findField p = preview p
