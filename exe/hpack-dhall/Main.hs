{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment (getArgs)
import Hpack (hpack, getOptions, setDecode)
import Hpack.Dhall (decodeFile, packageConfig)

main :: IO ()
main =
    getArgs
    >>= getOptions packageConfig
    >>= \ case
        Just (verbose, options) ->
            hpack verbose (setDecode decodeFile options)
        Nothing ->
            return ()
