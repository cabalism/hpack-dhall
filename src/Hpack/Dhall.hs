{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Hpack.Dhall where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           Data.Aeson
import qualified Data.Text.IO as T
import           System.Environment
import qualified Dhall.Parser
import qualified Dhall.Import
import qualified Dhall.TypeCheck
import qualified Dhall.JSON

import qualified Hpack

packageConfig :: FilePath
packageConfig = "package.dhall"

decodeDhall :: FilePath -> IO (Either String Value)
decodeDhall file = runExceptT $ do
  expr <- readInput >>= parseExpr >>= liftIO . Dhall.Import.load
  _ <- liftResult $ Dhall.TypeCheck.typeOf expr
  liftResult $ Dhall.JSON.dhallToJSON expr
  where
    readInput = liftIO (T.readFile file)
    parseExpr = liftResult . Dhall.Parser.exprFromText file
    liftResult = ExceptT . return . first show

main :: IO ()
main = do
  getArgs >>= Hpack.getOptions packageConfig >>= \ case
    Just (verbose, options) -> Hpack.hpack verbose (Hpack.setDecode decodeDhall options)
    Nothing -> return ()
