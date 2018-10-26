{-# LANGUAGE TupleSections #-}

module Hpack.Dhall (decodeExpr, decodeFile, packageConfig) where

import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Aeson (Value)
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T (readFile)
import Dhall.Core (Expr, Import)
import Dhall.Parser (Src, exprFromText)
import Dhall.Import (load)
import Dhall.TypeCheck (typeOf)
import Dhall.JSON (dhallToJSON)

type ParseExpr = Expr Src Import

packageConfig :: FilePath
packageConfig = "package.dhall"

decodeFile :: FilePath -> IO (Either String ([String], Value))
decodeFile file = liftIO (T.readFile file) >>= decodeExpr

decodeExpr :: T.Text -> IO (Either String ([String], Value))
decodeExpr contents = runExceptT $ do
    expr <- parseExpr contents >>= liftIO . load
    _ <- liftResult $ typeOf expr
    liftResult $ ([],) <$> dhallToJSON expr
    where
        liftResult :: (Show b, Monad m) => Either b a -> ExceptT String m a
        liftResult = ExceptT . return . first show

        parseExpr :: T.Text -> ExceptT String IO ParseExpr
        parseExpr = liftResult . exprFromText mempty
