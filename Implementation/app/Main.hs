{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Database.SQLite.Simple
import Lucid
import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid

type Server ctx = SpockCtxM ctx Connection () () ()
type Handler ctx = ActionCtxT ctx (WebStateM Connection () ()) ()

-- Database structure:
-- Flights: index, from, to
-- Seats: index, label, flight, cost, booked
-- Customers: index, name, email, password
-- InFlightServices: index, menuItem, seat
-- Menu: index, name, cost

main :: IO ()
main = do
  cfg <- defaultSpockCfg () (PCConn $ ConnBuilder (open "test.db") close (PoolCfg 1 1 1)) ()
  runSpock 8000 $ spock cfg app

runSql :: (HasSpock m, SpockConn m ~ Connection, ToRow q, FromRow r) => Query -> q -> m [r]
runSql q d = runQuery (\conn -> query conn q d)

app :: Server ()
app = do
  get root indexHandler

indexHandler :: Handler ()
indexHandler = error "not implemented"

