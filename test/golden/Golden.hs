{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import System.FilePath
    ( (</>), (<.>), (-<.>)
    , takeBaseName, replaceExtension
    , takeDirectory, splitDirectories, joinPath
    )
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (findByExtension)
import Test.Tasty.Golden (goldenVsFile, goldenVsString)

import Hpack (Verbose(..), Options(..), hpack, defaultOptions, setDecode)
import Hpack.Config (DecodeOptions(..))
import Hpack.Dhall (fileToJson, showDhall, showJson, showYaml)
import Data.ByteString.Lazy.UTF8 (fromString)

main :: IO ()
main =
    defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
    dhallFiles <- findByExtension [".dhall"] "test/golden/hpack-dhall-cabal/"
    return $ testGroup "golden tests"
        [ testGroup ".dhall to .cabal"
            [ goldenVsFile
                (testName dhallFile)
                (cabalFile <.> ".golden")
                cabalFile
                (writeCabal dhallFile)
            | dhallFile <- dhallFiles
            , let cabalFile = cabalFilePath dhallFile
            ]
        , testGroup ".dhall to dhall"
            [ goldenVsString
                (testName dhallFile)
                (dhallFile <.> ".golden")
                (fmap fromString . showDhall $ dhallFile)
            | dhallFile <- dhallFiles
            ]
        , testGroup ".dhall to json"
            [ goldenVsString
                (testName dhallFile)
                (dhallFile -<.> ".json")
                (fmap fromString . showJson $ dhallFile)
            | dhallFile <- dhallFiles
            ]
#if __GLASGOW_HASKELL__ >= 802
        , testGroup ".dhall to yaml"
            [ goldenVsString
                (testName dhallFile)
                (dhallFile -<.> ".yaml")
                (fmap fromString . showYaml $ dhallFile)
            | dhallFile <- dhallFiles
            ]
#endif
        ]

testName :: FilePath -> FilePath
testName p =
    if | fName == dName -> fName
       | fName == "package" -> dName
       | dName == "hpack-dhall-cabal" -> fName
       | otherwise -> dName </> fName
    where
        dName =
            joinPath
            . take 1
            . reverse
            . splitDirectories
            . takeDirectory
            $ p

        fName = takeBaseName p

writeCabal :: FilePath -> IO ()
writeCabal dhallFile =
    hpack NoVerbose (setDecode fileToJson options)
    where
        d = optionsDecodeOptions defaultOptions
        d' = d {decodeOptionsTarget = dhallFile}
        options = defaultOptions {optionsDecodeOptions = d'}

cabalFilePath :: FilePath -> FilePath
cabalFilePath p
    | takeBaseName p == "package" =
        case reverse . splitDirectories $ ds of
            d : ds' -> joinPath (reverse ds') </> d </> d <.> ".cabal"
            _ -> replaceExtension p ".cabal"

    | otherwise =
        replaceExtension p ".cabal"

    where
        ds = takeDirectory p
