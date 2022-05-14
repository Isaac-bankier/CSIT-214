{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Basics
import Data.HVect
import Database.SQLite.Simple
import Db
import Login
import UserManagement
import Web.Spock
import Web.Spock.Config
import Home
import BookFlight
import MyFlights

main :: IO ()
main = do
  conn <- open "/tmp/test.db"
  initDb conn
  close conn
  cfg <- defaultSpockCfg Nothing (PCConn $ ConnBuilder (open "/tmp/test.db") close (PoolCfg 1 1 1)) ()
  runSpock 8000 $ spock cfg app

app :: Server ()
app = prehook baseHook $ do
  prehook authHook $ get "/" home
  myFlights
  bookFlight
  login
