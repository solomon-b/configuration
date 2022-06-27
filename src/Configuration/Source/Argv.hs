{-# LANGUAGE ViewPatterns #-}

module Configuration.Source.Argv
  ( toArgParser,
    toParserInfo,
  )
where

--------------------------------------------------------------------------------

import Barbies
import Configuration.Setting (Arg (..), Setting (..), Source (..))
import Data.Bool (bool)
import Data.Functor.Compose (Compose (..))
import Data.Text qualified as T
import Options.Applicative qualified as Opt
import Prelude (Applicative (..), Functor (..), IO, Maybe (..), Read, Semigroup (..), ($))

--------------------------------------------------------------------------------

--TODO(SOLOMON): Figure out how to handle string like fields:

-- | Construct an Arg Parser from a @b Setting@. We allow for required
-- an 'Opt.Parser', an optional 'Opt.Parser', or no 'Opt.Parser' per
-- field.
buildParsers :: (AllB Read b, ConstraintsB b) => b Setting -> b (Compose Maybe (Compose Opt.Parser Maybe))
buildParsers = bmapC @Read $ \Setting {..} ->
  case optSource of
    Source _ _ (Just Arg {..}) _ ->
      Compose $
        pure $
          Compose $
            bool Opt.optional (fmap Just) argRequired $
              Opt.option
                Opt.auto
                ( Opt.long (T.unpack argFlag)
                    <> Opt.metavar (T.unpack argMetaVar)
                    <> Opt.help (T.unpack optDescription)
                )
    _ -> Compose Nothing

toParserInfo :: (TraversableB b) => b (Compose Maybe (Compose Opt.Parser Maybe)) -> Opt.ParserInfo (b Maybe)
toParserInfo b = Opt.info (btraverse f b) Opt.briefDesc
  where
    f :: Compose Maybe (Compose Opt.Parser Maybe) a -> Opt.Parser (Maybe a)
    f (getCompose -> Just (getCompose -> parser)) = parser
    f (getCompose -> Nothing) = pure Nothing

toArgParser :: (AllB Read b, TraversableB b, ConstraintsB b) => b Setting -> IO (b Maybe)
toArgParser options = Opt.execParser $ toParserInfo (buildParsers options)
