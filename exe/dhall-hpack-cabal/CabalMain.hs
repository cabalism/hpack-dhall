{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment (getArgs)
import Hpack (hpack, getOptions, setDecode)
import Hpack.Dhall (fileToJson, packageConfig)

main :: IO ()
main =
    getArgs
    >>= getOptions packageConfig
    >>= \ case
        Just (verbose, options) ->
            hpack verbose (setDecode fileToJson options)
        Nothing ->
            return ()
