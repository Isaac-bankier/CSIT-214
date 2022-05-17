{-# LANGUAGE OverloadedStrings #-}

module Db
  ( initDb
  , User(..)
  , Flight(..)
  , Seat(..)
  , Booking(..)
  , Service(..)
  , ServiceBooking(..)
  ) where

import qualified Data.Text as T
import Database.SQLite.Simple
import Control.Monad
import System.Random

initDb :: Connection -> IO ()
initDb c = do
  needsData <- fmap null (query_ c "SELECT name FROM sqlite_master" :: IO [[T.Text]])
  -- Setup users
  execute_ c "CREATE TABLE IF NOT EXISTS users (id INTEGER PRIMARY KEY, email TEXT, name TEXT, password TEXT, role TEXT);"
  -- Setup flights
  execute_ c "CREATE TABLE IF NOT EXISTS flights (id INTEGER PRIMARY KEY, fromCity TEXT, toCity TEXT, date TEXT);"
  -- Setup seats
  execute_ c "CREATE TABLE IF NOT EXISTS seats (id INTEGER PRIMARY KEY, flight INTEGER, name TEXT, cost INTEGER, FOREIGN KEY(flight) REFERENCES flights(id));"
  -- Setup bookings
  execute_ c "CREATE TABLE IF NOT EXISTS bookings (id INTEGER PRIMARY KEY, user INTEGER, seat INTEGER, FOREIGN KEY(user) REFERENCES users(id), FOREIGN KEY(seat) REFERENCES seats(id));"
  -- Setup services
  execute_ c "CREATE TABLE IF NOT EXISTS services (id INTEGER PRIMARY KEY, name TEXT, description TEXT, cost INTEGER);"
  -- Setup service bookings
  execute_ c "CREATE TABLE IF NOT EXISTS service_bookings (id INTEGER PRIMARY KEY, booking INTEGER, service INTEGER, FOREIGN KEY(booking) REFERENCES bookings(id), FOREIGN KEY(service) REFERENCES services(id));"
  when needsData $ addData c

addData :: Connection -> IO ()
addData c = do
  let users = [ ("alice@example.com" :: T.Text, "Alice" :: T.Text, "pass" :: T.Text, "customer" :: T.Text)
              , ("bob@example.com" :: T.Text, "Bob" :: T.Text, "pass" :: T.Text, "customer" :: T.Text)
              , ("eric@example.com" :: T.Text, "Eric" :: T.Text, "pass" :: T.Text, "employee" :: T.Text)
              ]
  forM_ users $ \d -> do
    execute c "INSERT INTO users (email, name, password, role) VALUES (?, ?, ?, ?);" d
  let flights = do
        let cities = ["Sydney" :: T.Text, "Melbourne", "Stockholm", "Las Vegas", "Constantinople"]
        from <- cities
        to <- cities
        guard $ from /= to
        day <- [1 :: Int, 5, 13, 17, 21, 25]
        month <- [1 :: Int ..12]
        return (from, to, T.concat [T.pack $ show day, "-", T.pack $ show month, "-2022"])
  forM_ flights $ \d -> do
    execute c "INSERT INTO flights (fromCity, toCity, date) VALUES (?, ?, ?);" d
  let seats = do
        (_, num) <- zip flights [1 :: Int ..]
        row <- [1 :: Int ..20]
        column <- ['A'..'F']
        return (num, show row ++ ['-', column])
  forM_ seats $ \(fnum, sname) -> do
    cost <- randomRIO (80 :: Int, 1000)
    execute c "INSERT INTO seats (flight, name, cost) VALUES (?, ?, ?);" (fnum, sname, cost)
  let services = [ ("Chicken Sandwich" :: T.Text, "Not great but what else will you get up here?" :: T.Text, 10 :: Int)
                 , ("Coffee", "$6 bucks for coffee‽", 6)
                 , ("Whiskey", "Nothing is better than flying drunk. Especially if you're the pilot.", 13)
                 ]
  forM_ services $ \d -> do
    execute c "INSERT INTO services (name, description, cost) VALUES (?, ?, ?);" d
  
data User = User {_userID :: Int, _email :: T.Text, _name :: T.Text, _password :: T.Text , _role :: T.Text}

instance FromRow User where
  fromRow = User <$> field <*> field <*> field <*> field <*> field

instance ToRow User where
  toRow (User i e n p r) = toRow (i, e, n, p, r)

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

data Service = Service {_serviceID :: Int, _serviceName :: T.Text, _description :: T.Text, _serviceCost :: Int}

instance FromRow Service where
  fromRow = Service <$> field <*> field <*> field <*> field

instance ToRow Service where
  toRow (Service i n d c) = toRow (i, n, d, c)

data ServiceBooking = ServiceBooking {_serviceBookingID :: Int, _bookingRef :: Int, _serviceRef :: Int}

instance FromRow ServiceBooking where
  fromRow = ServiceBooking <$> field <*> field <*> field

instance ToRow ServiceBooking where
  toRow (ServiceBooking i b s) = toRow (i, b, s)
