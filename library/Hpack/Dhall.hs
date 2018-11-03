{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

{-|
Module: Hpack.Dhall
Copyright:
    © 2018 Phil de Joux
    © 2018 Block Scope Limited
License: BSD3
Maintainer: Phil de Joux <phil.dejoux@blockscope.com>
Stability: experimental
Instead of working with <https://github.com/sol/hpack#readme hpack> in
<https://en.wikipedia.org/wiki/YAML YAML>, use
<https://github.com/dhall-lang/dhall-lang#readme Dhall>.  All functions resolve
imports relative to the location of the given @.dhall@ file.
-}
module Hpack.Dhall
    ( fileToJson
    , showJson
    , showYaml
    , showDhall
    , packageConfig
    ) where

import Data.Maybe (fromMaybe)
import Data.List (elemIndex)
import Data.String (IsString())
import Data.Function ((&))
import Lens.Micro ((^.), set)
import System.FilePath (takeDirectory)
import Control.Applicative (liftA2)
import Control.Exception (throwIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Bifunctor (first)
import Data.Aeson (ToJSON, Value)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text as T (Text, unpack)
import qualified Data.Text.IO as T (readFile)
import Dhall
    ( InputSettings, Text
    , rootDirectory, sourceName, defaultInputSettings
    )
import Dhall.Core (Expr)
import Dhall.Parser (Src, exprFromText)
import Dhall.Import (loadWith, emptyStatus)
import Dhall.TypeCheck (X, typeOf)
import Dhall.JSON (dhallToJSON)
import Dhall.Pretty (prettyExpr, layoutOpts)
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP
import qualified Data.Yaml.Pretty as Y

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

-- SEE: http://onoffswitch.net/adventures-pretty-printing-json-haskell/
getJson :: ToJSON a => a -> String
getJson = T.unpack . decodeUtf8 . BSL.toStrict . encodePretty

getYaml :: Value -> String
getYaml = T.unpack . decodeUtf8 . (Y.encodePretty cfg)
    where
        cfg = Y.setConfCompare cmp Y.defConfig

-- | The default package file name is @package.dhall@.
packageConfig :: FilePath
packageConfig = "package.dhall"

-- | Pretty prints JSON for the package description.
showJson
    :: FilePath -- ^ Path to a @.dhall@ file
    -> IO String
showJson file = do
    Right (_, v) <- fileToJson file
    return $ getJson v

-- | Pretty prints YAML for the package description.
showYaml
    :: FilePath -- ^ Path to a @.dhall@ file
    -> IO String
showYaml file = do
    Right (_, v) <- fileToJson file
    return $ getYaml v

-- | Pretty prints the package description Dhall expression, resolving imports
-- relative to the location of the @.dhall@ file.
showDhall
    :: FilePath -- ^ Path to a @.dhall@ file
    -> IO String
showDhall file = do
    text <- T.readFile file
    expr <- check (inputSettings file) text
    return . T.unpack $ renderDhall expr

-- | A file decoder for hpack. This should evaluate to a single record with
-- hpack's top-level <https://github.com/sol/hpack#top-level-fields fields>.
fileToJson
    :: FilePath -- ^ Path to a @.dhall@ file
    -> IO (Either String ([String], Value))
fileToJson file =
    liftIO (T.readFile file)
    >>= textToJson (inputSettings file)

inputSettings :: FilePath -> InputSettings
inputSettings file =
    Dhall.defaultInputSettings
    & set rootDirectory (takeDirectory file)
    & set sourceName file

textToJson
    :: InputSettings
    -> T.Text
    -> IO (Either String ([String], Value))
textToJson settings text = runExceptT $ do
    expr <- liftIO $ check settings text
    _ <- liftResult $ typeOf expr
    liftResult $ ([],) <$> dhallToJSON expr
    where
        liftResult :: (Show b, Monad m) => Either b a -> ExceptT String m a
        liftResult = ExceptT . return . first show

check :: InputSettings -> Text -> IO (Expr Src X)
check settings text = do
    expr <- either throwIO return $ exprFromText mempty text

    x <- State.evalStateT
            (loadWith expr)
            (emptyStatus $ settings ^. rootDirectory)

    return x

-- SEE: https://github.com/mstksg/hakyll-dhall
renderDhall :: (PP.Pretty a, Eq a) => Expr Src a -> T.Text
renderDhall =
    PP.renderStrict
    . PP.layoutSmart layoutOpts
    . PP.unAnnotate
    . prettyExpr
