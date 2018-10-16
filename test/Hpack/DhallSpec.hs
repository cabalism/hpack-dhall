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
        -- This file has been generated from package.dhall by hpack version #{showVersion Hpack.version}.
        --
        -- see: https://github.com/sol/hpack
        --
        -- hash: 19abf59677a84e0a8a49435b80f42f6007f6cdacd2b09c43514ab2b46d6c5927

        name:           foo
        version:        0.0.0
        build-type:     Simple
        cabal-version:  >= 1.10
        |]
