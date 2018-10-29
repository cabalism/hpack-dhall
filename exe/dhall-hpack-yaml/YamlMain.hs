{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Monoid ((<>))
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as O
import Options (Options(..), parseOptions)
import Hpack.Dhall (showYaml)

parserInfo :: ParserInfo Options
parserInfo =
    O.info
        parseOptions $
        O.fullDesc
        <> O.header "Hpack as YAML"
        <> O.progDesc "Show a package description as YAML."

main :: IO ()
main = do
    Options {..} <- O.execParser parserInfo
    s <- showYaml file
    putStrLn s
    return ()
