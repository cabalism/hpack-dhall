{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Foldable (forM_)
import Data.Version (showVersion)
import qualified Hpack as H (getOptions, version)
import Hpack.Dhall (packageConfig, showYaml)
import Options (Command (..), GlobOptions (..), parseGlobOptions, parserInfo)
import Options.Applicative (execParser)
import Paths_hpack_dhall (version)
import RecurseFile (resolveFile)
import System.FilePath (replaceExtension)

main :: IO ()
main =
  execParser (parserInfo parseOpts header description) >>= \case
    NumericVersion -> putStrLn $ showVersion version
    Version -> do
      putStrLn $ "cabal-diy-pack-" ++ showVersion version
      putStrLn $ "hpack-" ++ showVersion H.version
    Run GlobOptions{pkgGlobs = pfs, quiet, force, ignoreGlobs} -> do
      let includeGlobs = if null pfs then ["**/package.dhall"] else pfs
      pkgFiles <- resolveFile includeGlobs ignoreGlobs Nothing "."
      forM_ pkgFiles $ \pkgDhallFile -> do
        opts <- H.getOptions pkgDhallFile $ ["--force" | force] ++ ["--silent" | quiet]
        case opts of
          Just{} -> showYaml Nothing pkgDhallFile >>= writeFile (replaceExtension pkgDhallFile ".yaml")
          Nothing -> return ()
  where
    parseOpts = parseGlobOptions packageConfig
    header = "Cabal Dhall-Into-Yaml hook, for converting package.dhall into package.yaml."
    description = "Write the .cabal for a .dhall package description, resolving imports."
