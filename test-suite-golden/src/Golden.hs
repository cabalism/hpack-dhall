{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main (main) where

import System.Directory (renameFile)
import System.FilePath
  ( joinPath
  , replaceExtension
  , splitDirectories
  , takeBaseName
  , takeDirectory
  , (-<.>)
  , (<.>)
  , (</>)
  )
import System.Process (callProcess)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsFile, goldenVsString)
import Test.Tasty.HUnit (assertBool, testCase)

import Data.ByteString.Lazy.UTF8 (fromString)
import Hpack (Options (..), Verbose (..), defaultOptions, hpack, setDecode)
import Hpack.Config (DecodeOptions (..))
import Hpack.Dhall (dhallFileToJson, showDhall, showJson, showYaml)

data Out = Cabal | Dhall | Json | Yaml

main :: IO ()
main = defaultMain =<< goldenTests

{- FOURMOLU_DISABLE -}
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
{- FOURMOLU_ENABLE -}

goldenTests :: IO TestTree
goldenTests = do
  ks <- findByExtension [".dhall"] "test-suite-golden/test-files/key"
  rs <- findByExtension [".dhall"] "test-suite-golden/test-files/real-world"
  ms <- findByExtension [".dhall"] "test-suite-golden/test-files/maps"
  g1 <- goldenTestSet "archetypes" ks
  g2 <- goldenTestSet "real-world examples" rs
  -- The hpack and stack packages here are the ones from real-world, but written
  -- with toMap, so their golden files other than for .dhall are copies.
  g3 <- goldenTestSet "Dhall maps become objects" ms
  return $ testGroup "golden tests" [g1, g2, g3, canonicalTest, driftTests]

-- | Without @--canonical@, hpack tries to match the existing .cabal file, so
--   this would keep the layout of `perturbedCabal`.
canonicalTest :: TestTree
canonicalTest =
  goldenVsFile
    "output ignores the existing .cabal layout"
    (cabalFile -<.> goldExt Cabal)
    cabalFile
    ( do
        writeFile cabalFile perturbedCabal
        callProcess
          "dhall-hpack-cabal"
          ["--canonical", "--force", "--silent", "--package-dhall", dir </> "package.dhall"]
    )
  where
    dir = "test-suite-golden/test-files/canonical"
    cabalFile = dir </> "canonical.cabal"

-- | A example Cabal file with non-canonical field ordering.
perturbedCabal :: String
perturbedCabal =
  unlines
    [ "cabal-version: 1.12"
    , ""
    , "-- This file has been generated from package.dhall by hpack version 0.39.6."
    , "--"
    , "-- see: https://github.com/sol/hpack"
    , ""
    , "build-type: Simple"
    , "version: 0.0.0"
    , "synopsis: A package"
    , "name: canonical"
    ]

-- | The @.cabal@ golden files for the real-world packages written with toMap
-- should not drift from those of the originals. These tests come last so that
-- with @--accept@ they see the golden files after any changes are accepted.
driftTests :: TestTree
driftTests =
  testGroup
    "maps same as real-world"
    [ testCase name $ do
      maps <- readFile $ golden "maps"
      realWorld <- readFile $ golden "real-world"
      assertBool
        (golden "maps" ++ " differs from " ++ golden "real-world")
        (maps == realWorld)
    | name <- ["hpack", "stack"]
    , let golden dir =
            "test-suite-golden/test-files" </> dir </> name </> name <.> goldExt Cabal
    ]

goldenTestSet :: String -> [FilePath] -> IO TestTree
goldenTestSet title dhallFiles = do
  return $
    testGroup
      title
      [ testGroup
          ".dhall to .cabal"
          [ goldenVsFile
            (testName dhallFile)
            goldenFile
            cabalFile
            (writeDhallCabal dhallFile)
          | dhallFile <- dhallFiles
          , let cabalFile = cabalFilePath dhallFile
          , let goldenFile = cabalFile -<.> goldExt Cabal
          ]
      , testGroup
          ".dhall to dhall"
          [ goldenVsString
            (testName dhallFile)
            goldenFile
            (fmap fromString . showDhall $ dhallFile)
          | dhallFile <- dhallFiles
          , let goldenFile = dhallFile -<.> goldExt Dhall
          ]
      , testGroup
          ".dhall to json"
          [ goldenVsFile
            (testName dhallFile)
            goldenFile
            jsonFile
            (writeJson dhallFile jsonFile)
          | dhallFile <- dhallFiles
          , let jsonFile = dhallFile -<.> ".json"
          , let goldenFile = dhallFile -<.> goldExt Json
          ]
      , testGroup
          ".dhall to yaml"
          [ goldenVsFile
            (testName dhallFile)
            goldenFile
            yamlFile
            (writeYaml dhallFile yamlFile)
          | dhallFile <- dhallFiles
          , let yamlFile = dhallFile -<.> ".yaml"
          , let goldenFile = dhallFile -<.> goldExt Yaml
          ]
      , testGroup
          ".yaml to .cabal"
          [ goldenVsFile
            (testName dhallFile)
            goldenFile
            cabalFile
            (writeYamlCabal yamlFile cabalFile yamlCabalFile)
          | dhallFile <- dhallFiles
          , let yamlFile = dhallFile -<.> ".yaml"
          , let cabalFile = cabalFilePath dhallFile
          , let yamlCabalFile = yamlFile <.> ".cabal"
          , let goldenFile = cabalFile -<.> goldExt Cabal
          ]
      ]

testName :: FilePath -> FilePath
testName p =
  if
      | fName == dName -> fName
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
  hpack NoVerbose (setDecode dhallFileToJson options)
  where
    d = optionsDecodeOptions defaultOptions
    d' = d{decodeOptionsTarget = dhallFile}
    options = defaultOptions{optionsDecodeOptions = d'}

writeYamlCabal :: FilePath -> FilePath -> FilePath -> IO ()
writeYamlCabal yamlFile cabalFile yamlCabalFile = do
  renameFile cabalFile tmp
  hpack NoVerbose options
  renameFile cabalFile yamlCabalFile
  renameFile tmp cabalFile
  where
    tmp = cabalFile <.> ".TMP"

    d = optionsDecodeOptions defaultOptions
    d' = d{decodeOptionsTarget = yamlFile}
    options =
      defaultOptions
        { optionsDecodeOptions = d'
        }

writeJson :: FilePath -> FilePath -> IO ()
writeJson dhallFile jsonFile = do
  s <- showJson Nothing dhallFile
  writeFile jsonFile s
  appendFile jsonFile $ unlines [""]

writeYaml :: FilePath -> FilePath -> IO ()
writeYaml dhallFile yamlFile = do
  s <- showYaml Nothing dhallFile
  writeFile yamlFile s
  appendFile yamlFile $ unlines [""]

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
