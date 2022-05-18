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

bookFlight :: Server (HVect xs)
bookFlight = do
  prehook customerHook $ get "/bookFlights" findFlight
  prehook customerHook $ get ("/bookFlights" <//> var) $ \s -> findSeat s
  prehook customerHook $ post "/bookFlights" $ do
    sid <- param' "seatId"
    makeBooking sid

findFlight :: Handler (HVect (Customer ': xs)) a
findFlight = customerScaffold $ do
  flights <- lift $ runSqlQuery_ "SELECT * FROM flights"
  h1_ "Please select a flight to book"
  table_ $ do
    tr_ $ do
      td_ "From"
      td_ "To"
      td_ "Date"
      td_ "Book"
    foldr (*>) (return ()) $ fmap displayFight flights

findSeat :: Int -> Handler (HVect (Customer ': xs)) a
findSeat fid = customerScaffold $ do
  seats <- lift $ runSqlQuery "SELECT * FROM seats WHERE flight = ? AND NOT EXISTS (SELECT * FROM bookings WHERE seats.id = bookings.seat)" [fid]
  h1_ "Please select a seat to book"
  table_ $ do
    tr_ $ do
      td_ "Seat"
      td_ "Cost"
      td_ "Book"
    foldr (*>) (return ()) $ fmap displaySeat seats

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

displaySeat :: Monad m => Seat -> HtmlT m ()
displaySeat s = do
  tr_ $ do
    td_ $ toHtml $ _seatName s
    td_ $ toHtml $ show $ _cost s
    td_ $ form_ [method_ "post", action_ "/bookFlights/"] $ do
      input_ [type_ "hidden", name_ "seatId", value_ $ T.pack $ show $ _seatID s]
      input_ [type_ "submit", value_ "Purchase Seat"]
