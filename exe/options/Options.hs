{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Options
    ( Options(..)
    , parseNumericVersion
    , parseVersion
    , parseOptions
    , parsePkgFile
    , parseForce
    , parseQuiet
    ) where

import Hpack.Dhall (packageConfig)
import Options.Applicative

newtype Options = Options {pkgFile :: FilePath}

parseOptions :: Parser Options
parseOptions = helper <*> do
    pkgFile <- parsePkgFile
    return Options{..}

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

parseForce :: Parser Bool
parseForce =
    flag False True $
    long "force"
    <> short 'f'
    <> help "Overwrite of the output .cabal file unnecessarily"

parseQuiet :: Parser Bool
parseQuiet =
    flag False True $
    long "silent"
    <> help "Suppress logging"
