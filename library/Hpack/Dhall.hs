{-# LANGUAGE TupleSections #-}

{-|
Module: Hpack.Dhall
Copyright:
    © 2018 - 2021 Phil de Joux
    © 2018 - 2021 Block Scope Limited
License: BSD3
Maintainer: Phil de Joux <phil.dejoux@blockscope.com>
Stability: experimental
The functions in this module make it possible to configure an
<https://github.com/sol/hpack#readme hpack>
package description with
<https://github.com/dhall-lang/dhall-lang#readme Dhall>
instead of
<https://en.wikipedia.org/wiki/YAML YAML>.
When doing so, note that all functions resolve imports relative to the location
of the given @.dhall@ input file.
-}
module Hpack.Dhall
    ( fileToJson
    , showJson
    , showYaml
    , showDhall
    , packageConfig
    ) where

import Data.Void (Void)
import Data.Maybe (fromMaybe)
import Data.Function ((&))
import Lens.Micro ((^.), set)
import System.FilePath (takeDirectory)
import Control.Exception (throwIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Bifunctor (first)
import Data.Aeson (ToJSON, Value)
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
import Dhall.TypeCheck (typeOf)
import Dhall.JSON (dhallToJSON)
import Dhall.Pretty (prettyExpr, layoutOpts)
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PP
import qualified Data.Yaml.Pretty as Y
import qualified Data.Aeson.Encode.Pretty as A
import Hpack.Fields (cmp)

-- SEE: http://onoffswitch.net/adventures-pretty-printing-json-haskell/
getJson :: ToJSON a => (Text -> Text -> Ordering) -> a -> String
getJson cmp' =
    let cfg = A.defConfig {A.confCompare = cmp'}
    in T.unpack . decodeUtf8 . BSL.toStrict . A.encodePretty' cfg

getYaml :: ToJSON a => (Text -> Text -> Ordering) -> a -> String
getYaml cmp' =
    let cfg = Y.setConfCompare cmp' Y.defConfig
    in T.unpack . decodeUtf8 . Y.encodePretty cfg

-- | The default package file name is @package.dhall@.
packageConfig :: FilePath
packageConfig = "package.dhall"

-- | Pretty prints JSON for the package description.
showJson
    :: Maybe (Text -> Text -> Ordering)
    -- ^ An ordering of JSON fields.
    -> FilePath
    -- ^ Path to a @.dhall@ file
    -> IO String
showJson fieldOrdering file = do
    x <- fileToJson file
    return $ case x of
        Left err -> err
        Right (_, v) -> getJson (fromMaybe cmp fieldOrdering) v

-- | Pretty prints YAML for the package description.
showYaml
    :: Maybe (Text -> Text -> Ordering)
    -- ^ An ordering of YAML fields.
    -> FilePath
    -- ^ Path to a @.dhall@ file
    -> IO String
showYaml fieldOrdering file = do
    x <- fileToJson file
    return $ case x of
        Left err -> err
        Right (_, v) -> getYaml (fromMaybe cmp fieldOrdering) v

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

check :: InputSettings -> Text -> IO (Expr Src Void)
check settings text = do
    expr <- either throwIO return $ exprFromText mempty text
    State.evalStateT (loadWith expr) (emptyStatus $ settings ^. rootDirectory)

-- SEE: https://github.com/mstksg/hakyll-dhall
renderDhall :: PP.Pretty a => Expr Src a -> T.Text
renderDhall =
    PP.renderStrict
    . PP.layoutSmart layoutOpts
    . PP.unAnnotate
    . prettyExpr
