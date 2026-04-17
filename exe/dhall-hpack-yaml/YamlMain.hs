{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Version (showVersion)
import qualified Hpack as H (version)
import Hpack.Dhall (showYaml)
import Options (Command (..), Options (..), parseOptions, parserInfo)
import Options.Applicative (execParser)
import Paths_hpack_dhall (version)

main :: IO ()
main =
  execParser (parserInfo parseOptions header description) >>= \case
    NumericVersion -> putStrLn $ showVersion version
    Version -> do
      putStrLn $ "dhall-hpack-yaml-" ++ showVersion version
      putStrLn $ "hpack-" ++ showVersion H.version
    Run Options{..} -> showYaml Nothing pkgDhallFile >>= putStrLn
  where
    header = "Hpack as YAML"
    description = "Show a package description as YAML."
