{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Paths_hpack_dhall (version)
import Data.Version (showVersion)
import Data.Foldable (asum)
import qualified Options.Applicative as O
import Options (Options(..), parseOptions, parseNumericVersion, parseVersion)
import Hpack.Dhall (showDhall)
import qualified Hpack as H (version)

data Command = NumericVersion | Version | Run Options

parserInfo :: O.ParserInfo Command
parserInfo =
    O.info parser $
        O.fullDesc
        <> O.header "Hpack as Dhall"
        <> O.progDesc "Show a package description expression with imports resolved."
    where
        parser = asum
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
            putStrLn $ "dhall-hpack-dhall-" ++ showVersion version
            putStrLn $ "hpack-" ++ showVersion H.version

        Run Options{..} -> do
            s <- showDhall pkgFile
            putStrLn s
            return ()
