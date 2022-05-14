{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module BookFlight ( bookFlight ) where

import Basics
import Control.Monad.Trans
import Data.HVect hiding (singleton)
import qualified Data.Text as T
import Db
import Lucid
import SiteBuilders
import Util
import Web.Spock
import UserManagement

bookFlight :: Server (HVect xs)
bookFlight = do
  prehook authHook $ get "/bookFlights" findFlight
  prehook authHook $ get ("/bookFlights" <//> var) $ \s -> findSeat s
  prehook authHook $ post "/bookFlights" $ do
    sid <- param' "seatId"
    makeBooking sid

findFlight :: Handler (HVect xs) a
findFlight = mkSite $ scaffold $ do
  flights <- lift $ runSqlQuery_ "SELECT * FROM flights"
  h1_ "Please select a flight to book"
  table_ $ foldr (*>) (return ()) $ fmap displayFight flights

findSeat :: Int -> Handler (HVect xs) a
findSeat fid = mkSite $ scaffold $ do
  seats <- lift $ runSqlQuery "SELECT * FROM seats WHERE flight = ? AND NOT EXISTS (SELECT * FROM bookings WHERE seats.id = bookings.seat)" [fid]
  h1_ "Please select a seat to book"
  table_ $ foldr (*>) (return ()) $ fmap displaySeat seats

makeBooking :: Int -> Handler (HVect xs) a
makeBooking sid = maybeUser $ \case
  Just u -> do
    runSqlStmt "INSERT INTO bookings (user, seat) VALUES (?, ?)" (_userID u, sid)
    redirect "/myFlights"
  Nothing -> redirect "/login"


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
