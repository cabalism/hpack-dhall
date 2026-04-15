{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Paths_hpack_dhall (version)
import Data.Version (showVersion)
import Data.Foldable (asum)
import qualified Options.Applicative as O
import Options
    ( parsePkgYamlFile, parseNumericVersion, parseVersion
    , parseForce, parseQuiet
    )
import qualified Hpack as H (hpack, version, getOptions, Options(..))
import Hpack.Config (defaultDecodeOptions, DecodeOptions(..))

data Command = NumericVersion | Version | Run Options

data Options =
    Options
        { pkgYamlFile :: String
        , force :: Bool
        , quiet :: Bool
        }

parseOptions :: O.Parser Options
parseOptions = O.helper <*> do
    pkgYamlFile <- parsePkgYamlFile
    force <- parseForce
    quiet <- parseQuiet
    return Options{..}

parserInfo :: O.ParserInfo Command
parserInfo =
    O.info parser $
        O.fullDesc
        <> O.header "Hpack's dhalling"
        <> O.progDesc "Write the .cabal for a .yaml package description, same thing hpack does."
    where
        parser = asum
            [ NumericVersion <$ parseNumericVersion
            , Version <$ parseVersion
            , Run <$> parseOptions
            ]

main :: IO ()
main = O.execParser parserInfo >>= \case
    NumericVersion -> putStrLn $ showVersion version

    Version -> do
        putStrLn $ "dhall-hpack-cabal-" ++ showVersion version
        putStrLn $ "hpack-" ++ showVersion H.version

    Run Options{..} -> do
        opts <- H.getOptions pkgYamlFile $ [ "--force" | force ] ++ [ "--silent" | quiet ]
        case opts of
            Just (verbose, options) -> H.hpack verbose $
                options
                    {H.optionsDecodeOptions =
                        defaultDecodeOptions {decodeOptionsTarget = pkgYamlFile}
                    }
            Nothing -> return ()
