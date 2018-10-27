{-# LANGUAGE TupleSections #-}

module Hpack.Dhall (decodeExpr, decodeFile, packageConfig) where

import Data.Function ((&))
import Lens.Micro ((^.), set)
import System.FilePath (takeDirectory)
import Control.Exception (throwIO)
import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Monad.Trans.State.Strict as State
import Data.Bifunctor (first)
import Data.Aeson (Value)
import qualified Data.Text as T (Text)
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

packageConfig :: FilePath
packageConfig = "package.dhall"

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
