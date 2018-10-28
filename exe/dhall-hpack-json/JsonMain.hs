{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment (getArgs)
import Hpack.Dhall (showJson, packageConfig)

main :: IO ()
main =
    getArgs
    >>= \ case
            [s] -> showJson s
            _ -> showJson packageConfig
    >>= putStrLn
