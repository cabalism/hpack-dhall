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
        cabal-version: >= 1.10

        -- This file has been generated from package.dhall by hpack version #{showVersion Hpack.version}.
        --
        -- see: https://github.com/sol/hpack
        --
        -- hash: 98987c02e53bcdf76e4c3d2912c868d16f30f3115762548e41d337f436ffc1cc

        name:           foo
        version:        0.0.0
        build-type:     Simple
        |]
