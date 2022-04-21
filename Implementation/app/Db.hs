{-# LANGUAGE OverloadedStrings #-}

module Db where

import Control.Lens
import Data.Text
import Database.SQLite.Simple

-- Database structure:
-- Flights: index, from, to
-- Seats: index, label, flight, cost, booked
-- Customers: index, name, email, password
-- InFlightServices: index, menuItem, seat
-- Menu: index, name, cost
-- Managers: index, name, email, password


mkFlightTable = (`execute_` "CREATE TABLE IF NOT EXISTS Flights (index INTEGER PRIMARY KEY, name TEXT, from TEXT, to TEXT)")
data Flight = Flight {_flightIndex :: Int, _flightName:: Text, _flightFrom :: Text, _flightTo :: Text}
-- makeLenses ''Flight

instance FromRow Flight where
  fromRow = Flight <$> field <*> field <*> field <*> field

instance ToRow Flight where
  toRow (Flight i n f t) = toRow (i, n, f, t)

mkSeatTable = (`execute_` "CREATE TABLE IF NOT EXISTS Seats (index INTEGER PRIMARY KEY, label TEXT, flight INTEGER, to TEXT)")
data Seat = Seat {_seatIndex :: Int, _seatLabel :: Text, _seatFlight :: Int, _seatCost :: Int, _seatBooker :: Maybe Int}
-- makeLenses ''Seat

instance FromRow Seat where
  fromRow = Seat <$> field <*> field <*> field <*> field <*> field

instance ToRow Seat where
  toRow (Seat i l f c b) = toRow (i, l, f, c, b)

data Customer = Customer {_customerIndex :: Int, _customerName :: Text, _customerEmail :: Text, _customerPassword :: Text}
-- makeLenses ''Customer

instance FromRow Customer where
  fromRow = Customer <$> field <*> field <*> field <*> field

instance ToRow Customer where
  toRow (Customer i n e p) = toRow (i, n, e, p)

data ServiceBooking = ServiceBooking {_serviceBookingIndex :: Int, _serviceBookingItem :: Int, _serviceBookingSeat :: Int}
-- makeLenses ''ServiceBooking

instance FromRow ServiceBooking where
  fromRow = ServiceBooking <$> field <*> field <*> field

instance ToRow ServiceBooking where
  toRow (ServiceBooking i b s) = toRow (i, b, s)

data Service = Service {_serviceIndex :: Int, _serviceName :: Text, _serviceCost :: Int}
-- makeLenses ''Service

instance FromRow Service where
  fromRow = Service <$> field <*> field <*> field

instance ToRow Service where
  toRow (Service i n c) = toRow (i, n, c)

data Manager = Manager {_managerIndex :: Int, _managerName :: Text, _managerEmail :: Text, _managerPassword :: Text}
-- makeLenses ''Manager

instance FromRow Manager where
  fromRow = Manager <$> field <*> field <*> field <*> field

instance ToRow Manager where
  toRow (Manager i n e p) = toRow (i, n, e, p)
