{-# LANGUAGE NoMonomorphismRestriction #-}
module Hpack.Dhall where

import           Control.Monad.Trans.Except
import           Control.Monad.IO.Class
import           Data.Bifunctor
import           Data.Aeson
import qualified Data.Text.Lazy.IO as LT
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Text.Trifecta.Delta (Delta(..))
import qualified Dhall.Parser
import qualified Dhall.Import
import qualified Dhall.TypeCheck
import qualified Dhall.JSON

import qualified Hpack

packageConfig :: FilePath
packageConfig = "package.dhall"

readDhall :: FilePath -> IO (Either String Value)
readDhall file = runExceptT $ do
  expr <- readInput >>= parseExpr >>= liftIO . Dhall.Import.load
  _ <- liftResult $ Dhall.TypeCheck.typeOf expr
  liftResult $ Dhall.JSON.dhallToJSON expr
  where
    readInput = liftIO (LT.readFile file)
    parseExpr = liftResult . Dhall.Parser.exprFromText (Directed (encodeUtf8 $ T.pack file) 0 0 0 0)
    liftResult = ExceptT . return . first show

main :: IO ()
main = Hpack.mainWith packageConfig readDhall
