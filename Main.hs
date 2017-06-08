{-# LANGUAGE OverloadedStrings, QuasiQuotes,
             TemplateHaskell #-}
 
module Main where
import Yesod
import Yesod.Static
import Foundation
import Application
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Applicative
import Data.Text
import Database.Persist.Postgresql

connStr = "dbname=d2i8qse5af060m host=ec2-54-83-25-217.compute-1.amazonaws.com user=ewevpcekbnamil password=3cb36f1189e8160981ed8d6e4deb3bcf471656068d658028eb1bf0a4f1baa433 port=5432"

main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool 
       t@(Static settings) <- static "static"
       warp 8080 (Sitio t pool)