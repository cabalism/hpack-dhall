{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Options
    ( Options(..)
    , parseNumericVersion
    , parseVersion
    , parseOptions
    , parsePkgFile
    ) where

import Data.Monoid ((<>))
import Hpack.Dhall (packageConfig)
import Options.Applicative

data Options = Options {pkgFile :: FilePath}

parseOptions :: Parser Options
parseOptions = helper <*> do
    pkgFile <- parsePkgFile
    return (Options {..})

parsePkgFile :: Parser FilePath
parsePkgFile =
    strOption $
    long "package-dhall"
    <> metavar "FILE"
    <> value packageConfig
    <> showDefault
    <> help "A record of hpack fields"

parseNumericVersion :: Parser ()
parseNumericVersion =
    flag' () $
    long "numeric-version"
    <> help "Show version only"

parseVersion :: Parser ()
parseVersion =
    flag' () $
    long "version"
    <> help "Show app name and version"
