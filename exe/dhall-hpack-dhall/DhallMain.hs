{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Paths_hpack_dhall (version)
import Data.Version (showVersion)
import Options.Applicative (execParser)
import Options (Options(..), parserInfo, Command(..), parseOptions)
import Hpack.Dhall (showDhall)
import qualified Hpack as H (version)

main :: IO ()
main = execParser (parserInfo parseOptions header description) >>= \case
    NumericVersion -> putStrLn $ showVersion version

    Version -> do
        putStrLn $ "dhall-hpack-dhall-" ++ showVersion version
        putStrLn $ "hpack-" ++ showVersion H.version

    Run Options{..} -> showDhall pkgDhallFile >>= putStrLn
  where
    header = "Hpack as Dhall"
    description = "Show a package description expression with imports resolved."
