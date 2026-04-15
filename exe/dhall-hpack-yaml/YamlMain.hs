{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Paths_hpack_dhall (version)
import Data.Version (showVersion)
import Options.Applicative (execParser)
import Options (parserInfo, Command(..), Options(..), parseOptions)
import Hpack.Dhall (showYaml)
import qualified Hpack as H (version)

main :: IO ()
main = execParser (parserInfo parseOptions header description) >>= \case
    NumericVersion -> putStrLn $ showVersion version

    Version -> do
        putStrLn $ "dhall-hpack-yaml-" ++ showVersion version
        putStrLn $ "hpack-" ++ showVersion H.version

    Run Options{..} -> showYaml Nothing pkgDhallFile >>= putStrLn
  where
    header = "Hpack as YAML"
    description = "Show a package description as YAML."
