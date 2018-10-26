{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main (main) where

import Debug.Trace
import Data.Maybe (fromMaybe)
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Data.Function ((&))
import Lens.Micro (set)
import System.FilePath (takeBaseName, takeDirectory, replaceExtension)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Golden (findByExtension, goldenVsStringDiff)
import Test.Tasty.Golden.Advanced (goldenTest)

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString as BS
import qualified Data.Text.IO as StrictText
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Data.Text.Prettyprint.Doc.Render.Text as Pretty
import qualified Dhall
import qualified Dhall.Core
import qualified Distribution.InstalledPackageInfo as Cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Text as Cabal
import qualified Distribution.Version as Cabal
import qualified Distribution.PackageDescription.Parsec as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal
import qualified Distribution.Verbosity as Cabal
import Distribution.Types.GenericPackageDescription
    (GenericPackageDescription, emptyGenericPackageDescription)
import Hpack
    ( Verbose(..), Options(..)
    , hpack, hpackResult, defaultOptions, getOptions, setDecode
    )
import Hpack.Dhall (decodeExpr, packageConfig)
import Hpack.Config (Package)
import Hpack.Render (renderPackageWith, defaultRenderSettings)

main :: IO ()
main =
  defaultMain =<< goldenTests

goldenTests :: IO TestTree
goldenTests = do
    dhallFiles <- findByExtension [".dhall"] "golden-tests/dhall-to-cabal"
    return $ testGroup "golden tests"
        [ testGroup "dhall-to-cabal"
            [ goldenTest
                (takeBaseName dhallFile)
                (Cabal.readGenericPackageDescription Cabal.normal cabalFile)
                (StrictText.readFile dhallFile >>= pack settings)
                (\ (Cabal.showGenericPackageDescription -> exp) (Cabal.showGenericPackageDescription -> act) -> do
                  if exp == act then
                      return Nothing
                  else do
                    putStrLn $ "Diff between expected " ++ cabalFile ++
                               " and actual " ++ dhallFile ++ " :"
                    let gDiff = getGroupedDiff (lines exp) (lines act)
                    putStrLn $ ppDiff gDiff
                    return $ Just "Generated .cabal file does not match input"
                )
                (Cabal.writeGenericPackageDescription cabalFile)
            | dhallFile <- dhallFiles
            , let cabalFile = replaceExtension dhallFile ".cabal"
                  settings = Dhall.defaultInputSettings
                      & set Dhall.rootDirectory ( takeDirectory dhallFile )
                      & set Dhall.sourceName dhallFile
            ]
        ]

pack
    :: Dhall.InputSettings
    -> Dhall.Text
    -> IO GenericPackageDescription
pack _ t =
    return $ fromString pkg
    where
        pkg = renderPackage $ f t

f :: Dhall.Text -> Package
f = fromValue . decodeExpr

renderPackage :: Package -> String
renderPackage p =
    trace ("PKG: " ++ show p') p'
    where
        p' = renderPackageWith defaultRenderSettings 0 [] [] p

fromString :: String -> GenericPackageDescription
fromString s =
    fromMaybe emptyGenericPackageDescription x
    where
        x :: Maybe GenericPackageDescription
        x = Cabal.parseGenericPackageDescriptionMaybe $ C.pack s
