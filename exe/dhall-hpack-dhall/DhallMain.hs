{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Data.Monoid ((<>))
import Hpack.Dhall (showDhall, packageConfig)
import Options.Applicative (Parser, ParserInfo)
import qualified Options.Applicative as O

data Options = Options {file :: String}

parseOptions :: Parser Options
parseOptions = O.helper <*> do
    file <- parseFile
    return (Options {..})
  where
    parseFile =
        O.strOption $
        O.long "package-dhall"
        <> O.metavar "FILE"
        <> O.value packageConfig
        <> O.showDefault
        <> O.help "A record of hpack fields"

parserInfo :: ParserInfo Options
parserInfo =
    O.info
        parseOptions $
        O.fullDesc
        <> O.header "Hpack as Dhall"
        <> O.progDesc "Show the compiled dhall expression of an hpack package description file."

main :: IO ()
main = do
    Options {..} <- O.execParser parserInfo
    s <- showDhall file
    putStrLn s
    return ()
