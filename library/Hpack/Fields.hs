{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module Hpack.Fields
    ( cmp
    ) where

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Data.String (IsString())
import Control.Applicative (liftA2)

cmp :: (Ord a, IsString a) => a -> a -> Ordering
cmp a b =
    fromMaybe fallback $
        liftA2 compare (elemIndex a fields) (elemIndex b fields)
    where
        fallback =
            if | elem a fields -> LT
               | elem b fields -> GT
               | otherwise -> a `compare` b

        fields =
            [ "name"
            , "version"
            , "author"
            , "maintainer"
            , "copyright"
            , "license"
            , "license-file"
            , "category"
            , "synopsis"
            , "description"
            , "homepage"
            , "github"
            , "tested-with"
            , "extra-source-files"
            , "ghc-options"
            , "default-extensions"
            , "dependencies"
            , "main"
            , "source-dirs"
            , "library"
            , "executables"
            , "tests"
            , "when"
            , "condition"
            , "then"
            , "else"
            ]
