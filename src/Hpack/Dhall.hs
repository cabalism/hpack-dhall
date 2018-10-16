{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hpack.Dhall where

import Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Aeson (Value)
import qualified Data.Text as T (Text)
import qualified Data.Text.IO as T (readFile)
import System.Environment (getArgs)
import Dhall.Core (Expr , Import)
import qualified Dhall.Parser (Src, exprFromText)
import qualified Dhall.Import (load)
import qualified Dhall.TypeCheck (typeOf)
import qualified Dhall.JSON (dhallToJSON)

import qualified Hpack (hpack, getOptions, setDecode)

type ParseExpr = Expr Dhall.Parser.Src Import

packageConfig :: FilePath
packageConfig = "package.dhall"

decodeDhall :: FilePath -> IO (Either String Value)
decodeDhall file = runExceptT $ do
    expr <-
        liftIO (T.readFile file)
        >>= parseExpr
        >>= liftIO . Dhall.Import.load

    _ <- liftResult $ Dhall.TypeCheck.typeOf expr
    liftResult $ Dhall.JSON.dhallToJSON expr

liftResult :: (Show b, Monad m) => Either b a -> ExceptT String m a
liftResult = ExceptT . return . first show

parseExpr :: T.Text -> ExceptT String IO ParseExpr
parseExpr = liftResult . Dhall.Parser.exprFromText mempty

main :: IO ()
main = do
    getArgs
    >>= Hpack.getOptions packageConfig
    >>= \ case
        Just (verbose, options) ->
            Hpack.hpack verbose (Hpack.setDecode decodeDhall options)
        Nothing ->
            return ()
