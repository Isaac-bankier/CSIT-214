{-# LANGUAGE OverloadedStrings #-}

module Db
  ( initDb
  , User(..)
  , Flight (..)
  , Seat (..)
  , Booking (..)
  ) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Control.Monad

initDb :: Connection -> IO ()
initDb c = do
  -- Setup users
  execute_ c "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, email TEXT, name TEXT, password TEXT);"
  userRows <- query_ c "SELECT * FROM users" :: IO [User]
  when (null userRows) $ execute_ c "INSERT INTO users (email, name, password) VALUES (\"admin@admin.com\", \"admin\", \"pass\");"
  -- Setup flights
  execute_ c "CREATE TABLE IF NOT EXISTS flights (id INTEGER PRIMARY KEY, fromCity TEXT, toCity TEXT, date TEXT);"
  flightRows <- query_ c "SELECT * FROM flights" :: IO [Flight]
  when (null flightRows) $ foldr (*>) (return ())
    [ execute_ c "INSERT INTO flights (fromCity, toCity, date) VALUES (\"Sydney\", \"Melbourne\", \"13-07-2021\");"
    , execute_ c "INSERT INTO flights (fromCity, toCity, date) VALUES (\"Melbourne\", \"Sydney\", \"14-07-2021\");"
    ]
  -- Setup seats
  execute_ c "CREATE TABLE IF NOT EXISTS seats (id INTEGER PRIMARY KEY, flight INTEGER, name TEXT, cost INTEGER, FOREIGN KEY(flight) REFERENCES flights(id));"
  seatsRows <- query_ c "SELECT * FROM seats" :: IO [Seat]
  when (null seatsRows) $ foldr (*>) (return ())
    [ execute_ c "INSERT INTO seats (flight, name, cost) VALUES (1, \"1-A\", 100);"
    , execute_ c "INSERT INTO seats (flight, name, cost) VALUES (1, \"1-B\", 100);"
    , execute_ c "INSERT INTO seats (flight, name, cost) VALUES (2, \"2-A\", 100);"
    , execute_ c "INSERT INTO seats (flight, name, cost) VALUES (2, \"2-B\", 100);"
    ]
  -- Setup bookings
  execute_ c "CREATE TABLE IF NOT EXISTS bookings (id INTEGER PRIMARY KEY, user INTEGER, seat INTEGER, FOREIGN KEY(user) REFERENCES users(id), FOREIGN KEY(seat) REFERENCES seats(id));"

data User = User {_userID :: Int, _email :: T.Text, _name :: T.Text, _password :: T.Text }

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User i e n p) = toRow (i, e, n, p)

data Flight = Flight {_flightID :: Int, _from :: T.Text, _to :: T.Text, _date :: T.Text }

instance FromRow Flight where
  fromRow = Flight <$> field <*> field <*> field <*> field

instance ToRow Flight where
  toRow (Flight i f t d) = toRow (i, f, t, d)

data Seat = Seat {_seatID :: Int, _onFlight :: Int, _seatName :: T.Text, _cost :: Int }

instance FromRow Seat where
  fromRow = Seat <$> field <*> field <*> field <*> field

instance ToRow Seat where
  toRow (Seat i f n c) = toRow (i, f, n, c)

data Booking = Booking {_bookingID :: Int, _userRef :: Int, _seatRef :: Int}

instance FromRow Booking where
  fromRow = Booking <$> field <*> field <*> field

instance ToRow Booking where
  toRow (Booking i u s) = toRow (i, u, s)
