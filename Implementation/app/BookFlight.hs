{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module BookFlight ( bookFlight ) where

import Basics
import Control.Monad.Trans
import Data.HVect hiding (singleton)
import qualified Data.Text as T
import Db
import Lucid
    ( action_,
      form_,
      h1_,
      input_,
      method_,
      name_,
      table_,
      td_,
      tr_,
      type_,
      value_,
      HtmlT,
      ToHtml(toHtml) )
import SiteBuilders
import Util
import Web.Spock
import UserManagement
import qualified Data.HVect as H
import Control.Monad

bookFlight :: Server (HVect xs)
bookFlight = do
  prehook authHook $ get "/bookFlights" findFlight
  prehook authHook $ get ("/bookFlights" <//> var) $ \s -> findSeat s
  prehook customerHook $ post "/bookFlights" $ do
    sid <- param' "seatId"
    makeBooking sid

findFlight :: Handler (HVect (UserMode ': xs)) a
findFlight = scaffold $ do
  flights <- lift $ runSqlQuery_ "SELECT * FROM flights"
  h1_ "Please select a flight to book"
  table_ $ do
    tr_ $ do
      td_ "From"
      td_ "To"
      td_ "Date"
      td_ "Book"
    foldr (*>) (return ()) $ fmap displayFight flights

findSeat :: Int -> Handler (HVect (UserMode ': xs)) a
findSeat fid = scaffold $ do
  seats <- lift $ runSqlQuery "SELECT * FROM seats WHERE flight = ? AND NOT EXISTS (SELECT * FROM bookings WHERE seats.id = bookings.seat)" [fid]
  h1_ "Please select a seat to book"
  table_ $ do
    tr_ $ do
      td_ "Row/Column"
      td_ "A"
      td_ "B"
      td_ "C"
      td_ "D"
      td_ "E"
      td_ "F"
    foldr (*>) (return ()) $ fmap (processSeats seats) $ do
      row <- [1:: Int ..30]
      let tableRow = do
            column <- ['A'..'F']
            return $ show row ++ ['-', column]
      return (row, show row ++ "-A", show row ++ "-B", show row ++ "-C", show row ++ "-D", show row ++ "-E", show row ++ "-F")

makeBooking :: Int -> Handler (HVect (Customer ': xs)) a
makeBooking sid = do
  u <- H.head <$> getContext
  runSqlStmt "INSERT INTO bookings (user, seat) VALUES (?, ?)" (_userID u, sid)
  redirect "/myFlights"

displayFight :: Monad m => Flight -> HtmlT m ()
displayFight f = do
  tr_ $ do
    td_ $ toHtml $ _from f
    td_ $ toHtml $ _to f
    td_ $ toHtml $ _date f
    td_ $ form_ [method_ "get", action_ $ T.concat ["/bookFlights/", T.pack $ show $ _flightID f]] $ do
      input_ [type_ "submit", value_ "Select Seat"]

processSeats :: Monad m => [Seat] -> (Int, String, String, String, String, String, String) -> HtmlT m ()
processSeats seats (row, a, b, c, d, e, f) = do
  tr_ $ do
    td_ $ toHtml $ show row
    td_ $ ifIn $ T.pack a
    td_ $ ifIn $ T.pack b
    td_ $ ifIn $ T.pack c
    td_ $ ifIn $ T.pack d
    td_ $ ifIn $ T.pack e
    td_ $ ifIn $ T.pack f
  where ifIn s = case go s of
          [r] -> r
          _ -> "Taken"
        go x = do
          seat <- seats
          guard $ x == _seatName seat
          return $ do
            form_ [method_ "post", action_ "/bookFlights/"] $ do
              input_ [type_ "hidden", name_ "seatId", value_ $ T.pack $ show $ _seatID seat]
              input_ [type_ "submit", value_ $ T.pack $ '$':show (_cost seat)]
