{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Data.Monoid ((<>))
import Options.Applicative (ParserInfo)
import qualified Options.Applicative as O
import Options (Options(..), parseOptions)
import Hpack.Dhall (showDhall)

parserInfo :: ParserInfo Options
parserInfo =
    O.info
        parseOptions $
        O.fullDesc
        <> O.header "Hpack as Dhall"
        <> O.progDesc "Show a package description expression with imports resolved."

main :: IO ()
main = do
    Options {..} <- O.execParser parserInfo
    s <- showDhall file
    putStrLn s
    return ()
