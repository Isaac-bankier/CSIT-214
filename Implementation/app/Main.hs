{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import Basics
import BookFlight
import BookServices
import Css
import Database.SQLite.Simple
import Db
import EmployeeAct
import Home
import Login
import MyFlights
import Network.Wai.Middleware.Static
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

app :: Server ()
app = do
  middleware $ staticPolicy (addBase "assets")
  prehook baseHook $ do
    css
    prehook authHook $ get "/" home
    myFlights
    bookFlight
    bookService
    employeeAct
    login
