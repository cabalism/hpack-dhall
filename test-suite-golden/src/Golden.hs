{-# LANGUAGE MultiWayIf, LambdaCase, CPP #-}

module Main (main) where

import System.FilePath
    ( (</>), (<.>), (-<.>)
    , takeBaseName, replaceExtension
    , takeDirectory, splitDirectories, joinPath
    )
import System.Directory (renameFile)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsFile, goldenVsString)

import Hpack (Verbose(..), Options(..), hpack, defaultOptions, setDecode)
import Hpack.Config (DecodeOptions(..))
import Hpack.Dhall (fileToJson, showDhall, showJson, showYaml)
import Data.ByteString.Lazy.UTF8 (fromString)

data Out = Cabal | Dhall | Json | Yaml

main :: IO ()
main = defaultMain =<< goldenTests

goldExt :: Out -> FilePath
goldExt =
    \case
        Cabal -> ".cabal.golden"

        Dhall ->
            let d =
#if MIN_VERSION_dhall (1, 34, 0)
                    ".dhall-1.34"
#elif MIN_VERSION_dhall (1, 32, 0)
                    ".dhall-1.32"
#else
                    ".dhall"
#endif

            in d <> ".golden"

        Json -> ".json.golden"
        Yaml -> ".yaml.golden"

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
                (cabalFile -<.> goldExt Cabal)
                cabalFile
                (writeDhallCabal dhallFile)
            | dhallFile <- dhallFiles
            , let cabalFile = cabalFilePath dhallFile
            ]
        , testGroup ".dhall to dhall"
            [ goldenVsString
                (testName dhallFile)
                (dhallFile -<.> goldExt Dhall)
                (fmap fromString . showDhall $ dhallFile)
            | dhallFile <- dhallFiles
            ]
        , testGroup ".dhall to json"
            [ goldenVsFile
                (testName dhallFile)
                (dhallFile -<.> goldExt Json)
                jsonFile
                (writeJson dhallFile jsonFile)
            | dhallFile <- dhallFiles
            , let jsonFile = dhallFile -<.> ".json"
            ]
        , testGroup ".dhall to yaml"
            [ goldenVsFile
                (testName dhallFile)
                (dhallFile -<.> goldExt Yaml)
                yamlFile
                (writeYaml dhallFile yamlFile)
            | dhallFile <- dhallFiles
            , let yamlFile = dhallFile -<.> ".yaml"
            ]
        , testGroup ".yaml to .cabal"
            [ goldenVsFile
                (testName dhallFile)
                (cabalFile -<.> goldExt Cabal)
                cabalFile
                (writeYamlCabal yamlFile cabalFile yamlCabalFile)
            | dhallFile <- dhallFiles
            , let yamlFile = dhallFile -<.> ".yaml"
            , let cabalFile = cabalFilePath dhallFile
            , let yamlCabalFile = yamlFile <.> ".cabal"
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

writeDhallCabal :: FilePath -> IO ()
writeDhallCabal dhallFile =
    hpack NoVerbose (setDecode fileToJson options)
    where
        d = optionsDecodeOptions defaultOptions
        d' = d {decodeOptionsTarget = dhallFile}
        options = defaultOptions {optionsDecodeOptions = d'}

writeYamlCabal :: FilePath -> FilePath -> FilePath -> IO ()
writeYamlCabal yamlFile cabalFile yamlCabalFile = do
        renameFile cabalFile tmp
        hpack NoVerbose options
        renameFile cabalFile yamlCabalFile
        renameFile tmp cabalFile
    where
        tmp = cabalFile <.> ".TMP"

        d = optionsDecodeOptions defaultOptions
        d' = d {decodeOptionsTarget = yamlFile}
        options =
            defaultOptions
                { optionsDecodeOptions = d'
                }

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
