{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Paths_hpack_dhall (version)
import Data.Version (showVersion)
import Data.Foldable (forM_)
import Options.Applicative (execParser)
import Options (parserInfo, Command(..), GlobOptions(..), parseGlobOptions)
import qualified Hpack as H (hpack, version, getOptions, setDecode)
import Hpack.Dhall (dhallFileToJson)
import RecurseFile (resolveFile)

main :: IO ()
main = execParser (parserInfo parseGlobOptions header description) >>= \case
    NumericVersion -> putStrLn $ showVersion version

    Version -> do
        putStrLn $ "cabal-dpack-" ++ showVersion version
        putStrLn $ "hpack-" ++ showVersion H.version

    Run GlobOptions{pkgGlobs = pfs, quiet, force, ignoreGlobs} -> do
        let includeGlobs = if null pfs then ["**/package.dhall"] else pfs
        pkgFiles <- resolveFile includeGlobs ignoreGlobs Nothing "."
        forM_ pkgFiles $ \pkgFile -> do
            opts <- H.getOptions pkgFile $ [ "--force" | force ] ++ [ "--silent" | quiet ]
            case opts of
                Just (verbose, options) -> H.hpack verbose (H.setDecode dhallFileToJson options)
                Nothing -> return ()
  where
    header = "Cabal hook for converting package.dhall to <package-name>.cabal"
    description = "Write the .cabal for a .dhall package description, resolving imports."
