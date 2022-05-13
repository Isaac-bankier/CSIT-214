{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Basics
import Data.HVect
import Database.SQLite.Simple
import Db
import Login
import SiteBuilders
import UserManagement
import Web.Spock
import Web.Spock.Config

main :: IO ()
main = do
  conn <- open "/tmp/test.db"
  initDb conn
  close conn
  cfg <- defaultSpockCfg Nothing (PCConn $ ConnBuilder (open "/tmp/test.db") close (PoolCfg 1 1 1)) ()
  runSpock 8000 $ spock cfg app

baseHook :: Handler () (HVect '[])
baseHook = return HNil

app :: Server ()
app = prehook baseHook $ do
  prehook authHook $ get "/" $ mkSite "Hello"
  prehook authHook $ get "/about" $ mkSite "About"
  prehook authHook $ getpost "/logout" logout
  prehook loggedOutHook $ getpost "/login" login
