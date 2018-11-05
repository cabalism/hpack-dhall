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
    ks <- findByExtension [".dhall"] "test/golden/test-files/key"
    rs <- findByExtension [".dhall"] "test/golden/test-files/real-world"
    g1 <- goldenTestSet "archetypes" ks
    g2 <- goldenTestSet "real-world examples" rs
    return $ testGroup "golden tests" [g1 , g2]

goldenTestSet :: String -> [FilePath] -> IO TestTree
goldenTestSet title dhallFiles = do
    return $ testGroup title 
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
            [ goldenVsFile
                (testName dhallFile)
                (dhallFile -<.> ".json.golden")
                jsonFile
                (writeJson dhallFile jsonFile)
            | dhallFile <- dhallFiles
            , let jsonFile = dhallFile -<.> ".json"
            ]
        , testGroup ".dhall to yaml"
            [ goldenVsFile
                (testName dhallFile)
                (dhallFile -<.> ".yaml.golden")
                yamlFile
                (writeYaml dhallFile yamlFile)
            | dhallFile <- dhallFiles
            , let yamlFile = dhallFile -<.> ".yaml"
            ]
        ]

testName :: FilePath -> FilePath
testName p =
    if | fName == dName -> fName
       | fName == "package" -> dName
       | dName == "key" -> fName
       | dName == "real-world" -> fName
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

writeJson :: FilePath -> FilePath -> IO ()
writeJson dhallFile jsonFile = do
    s <- showJson Nothing dhallFile
    writeFile jsonFile s

writeYaml :: FilePath -> FilePath -> IO ()
writeYaml dhallFile yamlFile = do
    s <- showYaml Nothing dhallFile
    writeFile yamlFile s

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
