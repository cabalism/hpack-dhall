{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Paths_hpack_dhall (version)
import Data.Version (showVersion)
import Data.Foldable (forM_)
import Options.Applicative (execParser)
import Options (parserInfo, Command(..), GlobOptions(..), parseGlobOptions)
import qualified Hpack as H (hpack, version, getOptions, Options(..))
import Hpack.Config (defaultDecodeOptions, DecodeOptions(..))
import RecurseFile (resolveFile)

main :: IO ()
main = execParser (parserInfo parseGlobOptions header description) >>= \case
    NumericVersion -> putStrLn $ showVersion version

    Version -> do
        putStrLn $ "cabal-ypack-" ++ showVersion version
        putStrLn $ "hpack-" ++ showVersion H.version

    Run GlobOptions{pkgGlobs = pfs, quiet, force, ignoreGlobs} -> do
        let includeGlobs = if null pfs then ["**/package.yaml"] else pfs
        pkgFiles <- resolveFile includeGlobs ignoreGlobs Nothing "."
        forM_ pkgFiles $ \pkgYamlFile -> do
            opts <- H.getOptions pkgYamlFile $ [ "--force" | force ] ++ [ "--silent" | quiet ]
            case opts of
                Just (verbose, options) -> H.hpack verbose $
                    options
                        { H.optionsDecodeOptions =
                            defaultDecodeOptions {decodeOptionsTarget = pkgYamlFile}
                        }
                Nothing -> return ()
  where
    header = "Cabal hook for converting package.yaml to <package-name>.cabal"
    description = "Write the .cabal for a .dhall package description, resolving imports."
