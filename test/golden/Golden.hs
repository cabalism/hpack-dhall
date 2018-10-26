module Main (main) where

import System.FilePath ((<.>), takeBaseName, replaceExtension)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.Golden (goldenVsFile)

import Hpack (Verbose(..), Options(..), hpack, defaultOptions, setDecode)
import Hpack.Config (DecodeOptions(..))
import Hpack.Dhall (decodeFile)

main :: IO ()
main =
    defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
    dhallFiles <- findByExtension [".dhall"] "test/golden/hpack-dhall-cabal"
    return $ testGroup "golden tests"
        [ testGroup "hpack dhall to cabal"
            [ goldenVsFile
                (takeBaseName dhallFile)
                (cabalFile <.> ".golden")
                cabalFile
                (writeCabal dhallFile)
            | dhallFile <- dhallFiles
            , let cabalFile = replaceExtension dhallFile ".cabal"
            ]
        ]

writeCabal :: FilePath -> IO ()
writeCabal dhallFile =
    hpack Verbose (setDecode decodeFile options)
    where
        d = optionsDecodeOptions defaultOptions
        d' = d {decodeOptionsTarget = dhallFile}
        options = defaultOptions {optionsDecodeOptions = d'}
