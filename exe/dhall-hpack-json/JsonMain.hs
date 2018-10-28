{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment (getArgs)
import Hpack.Dhall (decodeToJson, packageConfig)

main :: IO ()
main =
    getArgs
    >>= \ case
            [s] -> decodeToJson s
            _ -> decodeToJson packageConfig
    >>= putStrLn
