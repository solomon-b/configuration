{-# LANGUAGE ViewPatterns #-}

module Configuration.Source.Argv
  ( toArgParser,
    toParserInfo,
  )
where

--------------------------------------------------------------------------------

import Barbies
import Configuration.Setting (Arg (..), Setting (..), Source (..))
import Data.Functor.Compose (Compose (..))
import Data.Text qualified as T
import Options.Applicative qualified as Opt
import Prelude (Applicative (..), IO, Maybe (..), Read, Semigroup (..), ($))

--------------------------------------------------------------------------------

--TODO(SOLOMON): Figure out how to handle string like fields:
-- | We want to allow for required parsers, optional parsers, and no
-- parser per field.
toArgParser' :: (AllB Read b, ConstraintsB b) => b Setting -> b (Compose Maybe Opt.Parser)
toArgParser' = bmapC @Read $ \Setting {..} ->
  case optSource of
    Source _ _ (Just Arg {..}) ->
      Compose $
        pure $
          Opt.option
            Opt.auto
            ( Opt.long (T.unpack argFlag)
                <> Opt.metavar (T.unpack argMetaVar)
                <> Opt.help (T.unpack optDescription)
            )
    _ -> Compose Nothing

toParserInfo :: (TraversableB b) => b (Compose Maybe Opt.Parser) -> Opt.ParserInfo (b Maybe)
toParserInfo b = Opt.info (btraverse f b) Opt.briefDesc
  where
    f :: Compose Maybe Opt.Parser a -> Opt.Parser (Maybe a)
    f (getCompose -> Just parser) = Opt.optional parser
    f (getCompose -> Nothing) = pure Nothing

--TODO(SOLOMON): Figure out how to allow for required argv parsers
toArgParser :: (AllB Read b, TraversableB b, ConstraintsB b) => b Setting -> IO (b Maybe)
toArgParser options = Opt.execParser $ toParserInfo (toArgParser' options)
