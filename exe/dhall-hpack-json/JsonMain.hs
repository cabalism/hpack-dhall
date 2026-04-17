{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Version (showVersion)
import qualified Hpack as H (version)
import Hpack.Dhall (showJson)
import Options (Command (..), Options (..), parseOptions, parserInfo)
import Options.Applicative (execParser)
import Paths_hpack_dhall (version)

main :: IO ()
main =
  execParser (parserInfo parseOptions header description) >>= \case
    NumericVersion -> putStrLn $ showVersion version
    Version -> do
      putStrLn $ "dhall-hpack-json-" ++ showVersion version
      putStrLn $ "hpack-" ++ showVersion H.version
    Run Options{..} -> showJson Nothing pkgDhallFile >>= putStrLn
  where
    header = "Hpack as JSON"
    description = "Show a package description as JSON."
