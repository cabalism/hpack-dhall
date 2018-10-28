{-# LANGUAGE TupleSections #-}

module Hpack.Dhall
    ( fileToJson
    , showJson
    , showYaml
    , showDhall
    , packageConfig
    ) where

import Data.Function ((&))
import Lens.Micro ((^.), set)
import System.FilePath (takeDirectory)
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
import Data.Yaml (encode)
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP

-- SEE: http://onoffswitch.net/adventures-pretty-printing-json-haskell/
getJson :: ToJSON a => a -> String
getJson = T.unpack . decodeUtf8 . BSL.toStrict . encodePretty

getYaml :: ToJSON a => a -> String
getYaml = T.unpack . decodeUtf8 . Data.Yaml.encode

packageConfig :: FilePath
packageConfig = "package.dhall"

showJson :: FilePath -> IO String
showJson file = do
    Right (_, v) <- fileToJson file
    return $ getJson v

showYaml :: FilePath -> IO String
showYaml file = do
    Right (_, v) <- fileToJson file
    return $ getYaml v

showDhall :: FilePath -> IO String
showDhall file = do
    text <- T.readFile file
    expr <- check (inputSettings file) text
    return . T.unpack $ renderDhall expr

fileToJson :: FilePath -> IO (Either String ([String], Value))
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
