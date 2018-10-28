{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment (getArgs)
import Hpack.Dhall (showDhall, packageConfig)

main :: IO ()
main =
    getArgs
    >>= \ case
            [s] -> showDhall s
            _ -> showDhall packageConfig
    >>= putStrLn
