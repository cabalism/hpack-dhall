{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Options
  ( Options (..)
  , parseNumericVersion
  , parseVersion
  , parseOptions
  , parsePkgYamlFile
  , parsePkgDhallFile
  , parseForce
  , parseQuiet
  , parserInfo
  , Command (..)

    -- ** Globs
  , GlobOptions (..)
  , parsePkgGlobs
  , parseIgnoreGlobs
  , parseGlobOptions
  ) where

import qualified Hpack.Config as HpackYaml (packageConfig)
import qualified Hpack.Dhall as HpackDhall (packageConfig)
import Options.Applicative

newtype Options = Options {pkgDhallFile :: FilePath}

data Command a = NumericVersion | Version | Run a

parserInfo :: Parser a -> String -> String -> ParserInfo (Command a)
parserInfo parseOpts h d =
  info parser $
    fullDesc
      <> header h
      <> progDesc d
  where
    parser =
      asum
        [ NumericVersion <$ parseNumericVersion
        , Version <$ parseVersion
        , Run <$> parseOpts
        ]

data GlobOptions = GlobOptions
  { force :: Bool
  , quiet :: Bool
  , pkgGlobs :: [String]
  , ignoreGlobs :: [String]
  }

parseGlobOptions :: String -> Parser GlobOptions
parseGlobOptions filename =
  helper <*> do
    force <- parseForce
    quiet <- parseQuiet
    pkgGlobs <- parsePkgGlobs filename
    ignoreGlobs <- parseIgnoreGlobs filename
    return GlobOptions{..}

parseOptions :: Parser Options
parseOptions =
  helper <*> do
    pkgDhallFile <- parsePkgDhallFile
    return Options{..}

parsePkgYamlFile :: Parser FilePath
parsePkgYamlFile =
  strOption $
    long "package-yaml"
      <> metavar "FILE"
      <> value HpackYaml.packageConfig
      <> showDefault
      <> help "A record of hpack fields"

parsePkgDhallFile :: Parser FilePath
parsePkgDhallFile =
  strOption $
    long "package-dhall"
      <> metavar "FILE"
      <> value HpackDhall.packageConfig
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

parsePkgGlobs :: String -> Parser [String]
parsePkgGlobs name =
  many . strOption $
    long (map (\c -> if c == '.' then '-' else c) name)
      <> metavar "GLOB"
      <> help ("Glob pattern to include when searching for '" ++ name ++ "' (or otherwise named) package files. Can be specified multiple times.")

parseIgnoreGlobs :: String -> Parser [String]
parseIgnoreGlobs name =
  many . strOption $
    long "ignore-glob"
      <> short 'i'
      <> metavar "GLOB"
      <> help ("Glob pattern to ignore when searching for '" ++ name ++ "' files. Can be specified multiple times.")
