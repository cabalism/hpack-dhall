{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Monoid ((<>))
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as O
import Options (Options(..), parseOptions)
import Hpack.Dhall (showJson)

parserInfo :: ParserInfo Options
parserInfo =
    O.info
        parseOptions $
        O.fullDesc
        <> O.header "Hpack as JSON"
        <> O.progDesc "Show a package description as JSON."

main :: IO ()
main = do
    Options {..} <- O.execParser parserInfo
    s <- showJson file
    putStrLn s
    return ()
