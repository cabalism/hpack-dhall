{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}

module Main (main) where

import Paths_hpack_dhall (version)
import Data.Version (showVersion)
import Data.Foldable (asum)
import qualified Options.Applicative as O
import Options
    ( parsePkgFile, parseNumericVersion, parseVersion
    , parseForce, parseQuiet
    )
import qualified Hpack as H (hpack, version, getOptions, setDecode)
import Hpack.Dhall (fileToJson)

data Command = NumericVersion | Version | Run Options

data Options =
    Options
        { pkgFile :: String
        , force :: Bool
        , quiet :: Bool
        }

parseOptions :: O.Parser Options
parseOptions = O.helper <*> do
    pkgFile <- parsePkgFile
    force <- parseForce
    quiet <- parseQuiet
    return Options{..}

parserInfo :: O.ParserInfo Command
parserInfo =
    O.info parser $
        O.fullDesc
        <> O.header "Hpack's dhalling"
        <> O.progDesc "Write the .cabal for a .dhall package description, resolving imports."
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
            putStrLn $ "dhall-hpack-cabal-" ++ showVersion version
            putStrLn $ "hpack-" ++ showVersion H.version

        Run Options{..} -> do
            opts <- H.getOptions pkgFile $
                mconcat
                    [ [ "--force" | force ]
                    , [ "--silent" | quiet ]
                    ]
            case opts of
                Just (verbose, options) ->
                    H.hpack verbose (H.setDecode fileToJson options)
                Nothing ->
                    return ()
