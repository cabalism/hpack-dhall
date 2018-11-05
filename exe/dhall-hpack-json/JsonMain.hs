{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Paths_hpack_dhall (version)
import Data.Version (showVersion)
import Data.Monoid ((<>))
import Data.Foldable (asum)
import qualified Options.Applicative as O
import Options (Options(..), parseOptions, parseNumericVersion, parseVersion)
import Hpack.Dhall (showJson)
import qualified Hpack as H (version)

data Command = NumericVersion | Version | Run Options

parserInfo :: O.ParserInfo Command
parserInfo =
    O.info parser $
        O.fullDesc
        <> O.header "Hpack as JSON"
        <> O.progDesc "Show a package description as JSON."
    where
        parser = asum $
            [ NumericVersion <$ parseNumericVersion
            , Version <$ parseVersion
            , Run <$> parseOptions
            ]

main :: IO ()
main = do
    command <- O.execParser parserInfo

    case command of
        NumericVersion ->
            putStrLn $ showVersion version

        Version -> do
            putStrLn $ "dhall-hpack-json-" ++ showVersion version
            putStrLn $ "hpack-" ++ showVersion H.version

        Run (Options {..}) -> do
            s <- showJson Nothing pkgFile
            putStrLn s
            return ()
