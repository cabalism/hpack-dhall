{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Foldable (forM_)
import Data.Version (showVersion)
import qualified Hpack as H (getOptions, hpack, setDecode, version)
import Hpack.Dhall (dhallFileToJson, packageConfig)
import Options (Command (..), GlobOptions (..), parseGlobOptions, parserInfo)
import Options.Applicative (execParser)
import Paths_hpack_dhall (version)
import RecurseFile (resolveFile)

main :: IO ()
main =
  execParser (parserInfo parseOpts header description) >>= \case
    NumericVersion -> putStrLn $ showVersion version
    Version -> do
      putStrLn $ "cabal-dpack-" ++ showVersion version
      putStrLn $ "hpack-" ++ showVersion H.version
    Run GlobOptions{pkgGlobs = pfs, quiet, force, ignoreGlobs} -> do
      let includeGlobs = if null pfs then ["**/package.dhall"] else pfs
      pkgFiles <- resolveFile includeGlobs ignoreGlobs Nothing "."
      forM_ pkgFiles $ \pkgFile -> do
        opts <- H.getOptions pkgFile $ ["--force" | force] ++ ["--silent" | quiet]
        case opts of
          Just (verbose, options) -> H.hpack verbose (H.setDecode dhallFileToJson options)
          Nothing -> return ()
  where
    parseOpts = parseGlobOptions packageConfig
    header = "Cabal hook for converting package.dhall to <package-name>.cabal"
    description = "Write the .cabal for a .dhall package description, resolving imports."
