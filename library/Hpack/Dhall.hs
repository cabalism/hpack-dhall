{-# LANGUAGE TupleSections #-}

module Hpack.Dhall
    ( decodeExpr
    , decodeFile
    , decodeToJson
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

-- SEE: http://onoffswitch.net/adventures-pretty-printing-json-haskell/
getJson :: ToJSON a => a -> String
getJson d = T.unpack $ decodeUtf8 $ BSL.toStrict (encodePretty d)

packageConfig :: FilePath
packageConfig = "package.dhall"

decodeToJson :: FilePath -> IO String 
decodeToJson file = do
    Right (_, v) <- decodeFile file
    return $ getJson v

decodeFile :: FilePath -> IO (Either String ([String], Value))
decodeFile file = liftIO (T.readFile file) >>= decodeExpr settings
    where
        settings =
            Dhall.defaultInputSettings
            & set rootDirectory (takeDirectory file)
            & set sourceName file

decodeExpr
    :: InputSettings
    -> T.Text
    -> IO (Either String ([String], Value))
decodeExpr settings text = runExceptT $ do
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
