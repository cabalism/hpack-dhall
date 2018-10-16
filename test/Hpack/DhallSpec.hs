{-# LANGUAGE QuasiQuotes #-}
module Hpack.DhallSpec (spec) where

import           Test.Hspec
import           Test.Mockery.Directory
import           Data.String.Interpolate
import           Data.String.Interpolate.Util

import           Data.Version (showVersion)
import qualified Hpack

import           Hpack.Dhall

spec :: Spec
spec = do
  describe "main" $ do
    it "generates cabal files" $ do
      inTempDirectory $ do
        writeFile packageConfig [i|
        {
          name = "foo"
        }
        |]
        main
        readFile "foo.cabal" `shouldReturn` unindent [i|
        cabal-version: 1.12

        -- This file has been generated from package.dhall by hpack version #{showVersion Hpack.version}.
        --
        -- see: https://github.com/sol/hpack
        --
        -- hash: b58171d1cda9f003bffcf5578533939876104566ce483a27ed613ccc1caf55c0

        name:           foo
        version:        0.0.0
        build-type:     Simple
        |]
