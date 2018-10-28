{-# LANGUAGE LambdaCase #-}

module Main (main) where

import System.Environment (getArgs)
import Hpack.Dhall (showYaml, packageConfig)

main :: IO ()
main =
    getArgs
    >>= \ case
            [s] -> showYaml s
            _ -> showYaml packageConfig
    >>= putStrLn
