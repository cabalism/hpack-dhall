{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Options where

import Data.Monoid ((<>))
import Hpack.Dhall (packageConfig)
import Options.Applicative

data Options = Options {file :: String}

parseOptions :: Parser Options
parseOptions = helper <*> do
    file <- parseFile
    return (Options {..})
  where
    parseFile =
        strOption $
        long "package-dhall"
        <> metavar "FILE"
        <> value packageConfig
        <> showDefault
        <> help "A record of hpack fields"
