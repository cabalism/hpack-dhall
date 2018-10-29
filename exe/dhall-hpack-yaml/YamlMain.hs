{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Paths_hpack_dhall (version)
import Data.Version (showVersion)
import Data.Monoid ((<>))
import Data.Foldable (asum)
import qualified Options.Applicative as O
import Options (Options(..), parseOptions, parseNumericVersion, parseVersion)
import Hpack.Dhall (showYaml)
import qualified Hpack as H (version)

data Command = Run Options | Version | NumericVersion

parserInfo :: O.ParserInfo Command
parserInfo =
    O.info parser $
        O.fullDesc
        <> O.header "Hpack as YAML"
        <> O.progDesc "Show a package description as YAML."
    where
        parser = asum $
            [ Run <$> parseOptions
            , Version <$ parseVersion
            , NumericVersion <$ parseNumericVersion
            ]

main :: IO ()
main = do
    command <- O.execParser parserInfo

    case command of
        Run (Options {..}) -> do
            s <- showYaml file
            putStrLn s
            return ()

        Version -> do
            putStrLn $ "dhall-hpack-yaml-" ++ showVersion version
            putStrLn $ "hpack-" ++ showVersion H.version

        NumericVersion ->
            putStrLn $ showVersion version
