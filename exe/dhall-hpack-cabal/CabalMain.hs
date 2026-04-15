{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Paths_hpack_dhall (version)
import Data.Version (showVersion)
import qualified Options.Applicative as O
import Options (parsePkgDhallFile, parseForce, parseQuiet, Command(..), parserInfo)
import qualified Hpack as H (hpack, version, getOptions, setDecode)
import Hpack.Dhall (dhallFileToJson)

data Options =
    Options
        { pkgDhallFile :: String
        , force :: Bool
        , quiet :: Bool
        }

parseOptions :: O.Parser Options
parseOptions = O.helper <*> do
    pkgDhallFile <- parsePkgDhallFile
    force <- parseForce
    quiet <- parseQuiet
    return Options{..}

main :: IO ()
main = O.execParser (parserInfo parseOptions header description) >>= \case
    NumericVersion -> putStrLn $ showVersion version

    Version -> do
        putStrLn $ "dhall-hpack-cabal-" ++ showVersion version
        putStrLn $ "hpack-" ++ showVersion H.version

    Run Options{..} -> do
        opts <- H.getOptions pkgDhallFile $ [ "--force" | force ] ++ [ "--silent" | quiet ]
        case opts of
            Just (verbose, options) -> H.hpack verbose (H.setDecode dhallFileToJson options)
            Nothing -> return ()
  where
    header = "Hpack's dhalling"
    description =  "Write the .cabal for a .dhall package description, resolving imports."
