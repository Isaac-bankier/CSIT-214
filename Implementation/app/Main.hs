{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Basics
import Data.HVect
import Database.SQLite.Simple
import Login
import SiteBuilders
import UserManagement
import Web.Spock
import Web.Spock.Config


main :: IO ()
main = do
  cfg <- defaultSpockCfg Nothing (PCConn $ ConnBuilder (open "/tmp/test.db") close (PoolCfg 1 1 1)) ()
  runSpock 8000 $ spock cfg app

baseHook :: Handler () (HVect '[])
baseHook = return HNil

app :: Server ()
app = prehook baseHook $ do
  get "/" $ mkSite "Hello"
  get "/about" $ mkSite "About"
  getpost "/login" login
  getpost "/logout" logout
  prehook authHook $ get "/authorised" $ mkSite "Authorised"
