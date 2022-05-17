{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module MyFlights ( myFlights ) where

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
import Database.SQLite.Simple
import qualified Data.HVect as H

myFlights :: Server (HVect xs)
myFlights = do
  prehook customerHook $ get "/myFlights" listFlights
  prehook customerHook $ post "/cancelBooking" $ do
    bid <- param' "bookingId"
    cancelBooking bid
  prehook customerHook $ post "/cancelServiceBooking" $ do
    sbid <- param' "serviceBookingId"
    cancelServiceBooking sbid

listFlights :: Handler (HVect (Customer ': xs)) a
listFlights = do
  u <- H.head <$> getContext
  flights <- lift $ runSqlQuery "SELECT * FROM bookings WHERE user = ?" [_userID u]
  services <- lift $ runSqlQuery "SELECT * FROM service_bookings WHERE EXISTS (SELECT * FROM bookings WHERE bookings.user = ? AND service_bookings.booking = bookings.id)" [_userID u]
  customerScaffold $ do
    h1_ "Flights"
    case flights of
      [] -> h2_ "You haven't booked any flights."
      _ -> table_ $ foldr (*>) (return ()) $ fmap displayBooking flights
    h1_ "Services"
    case services of
      [] -> h2_ "You haven't booked any services."
      _ -> table_ $ foldr (*>) (return ()) $ fmap displayService services

cancelBooking :: Int -> Handler (HVect (Customer ': xs)) a
cancelBooking bid = do
  runSqlStmt "DELETE FROM bookings WHERE id = ?" [bid]
  redirect "/myFlights"

cancelServiceBooking :: Int -> Handler (HVect xs) a
cancelServiceBooking sbid = do
  runSqlStmt "DELETE FROM service_bookings WHERE id = ?" [sbid]
  redirect "/myFlights"

displayBooking :: (Monad m, HasSpock m, SpockConn m ~ Connection) => Booking -> HtmlT m ()
displayBooking b = do
  tr_ $ do
    s <- lift $ runSqlQuery "SELECT * FROM seats WHERE id = ?" [_seatRef b]
    case s of
      [s'@Seat {}] -> do
        f <- lift $ runSqlQuery "SELECT * FROM flights WHERE id = ?" [_onFlight s']
        case f of
          [f'@Flight {}] -> do
            td_ $ toHtml $ _from f'
            td_ $ toHtml $ _to f'
            td_ $ toHtml $ _date f'
            td_ $ toHtml $ _seatName s'
            td_ $ toHtml $ show $ _cost s'
            td_ $ form_ [method_ "post", action_ "/cancelBooking"] $ do
              input_ [type_ "hidden", name_ "bookingId", value_ $ T.pack $ show $ _bookingID b]
              input_ [type_ "submit", value_ "Cancel Booking"]
          _ -> error "The database reached an invalid state."
      _ -> error "The database reached an invalid state."

displayService :: (Monad m, HasSpock m, SpockConn m ~ Connection) => ServiceBooking -> HtmlT m ()
displayService sb = do
  tr_ $ do
    sv <- lift $ runSqlQuery "SELECT * FROM services WHERE id = ?" [_serviceRef sb]
    case sv of
      [sv'@Service {}] -> do
        b <- lift $ runSqlQuery "SELECT * FROM bookings WHERE id = ?" [_bookingRef sb]
        case b of
          [b'@Booking {}] -> do
            s <- lift $ runSqlQuery "SELECT * FROM seats WHERE id = ?" [_seatRef b']
            case s of
              [s'@Seat {}] -> do
                f <- lift $ runSqlQuery "SELECT * FROM flights WHERE id = ?" [_onFlight s']
                case f of
                  [f'@Flight {}] -> do
                    td_ $ toHtml $ _serviceName sv'
                    td_ $ toHtml $ _description sv'
                    td_ $ toHtml $ _from f'
                    td_ $ toHtml $ _to f'
                    td_ $ toHtml $ _seatName s'
                    td_ $ toHtml $ show $ _serviceCost sv'
                    td_ $ form_ [method_ "post", action_ "/cancelServiceBooking"] $ do
                      input_ [type_ "hidden", name_ "serviceBookingId", value_ $ T.pack $ show $ _serviceBookingID sb]
                      input_ [type_ "submit", value_ "Cancel Service"]
                  _ -> error "The database reached an invalid state."
              _ -> error "The database reached an invalid state."
          _ -> error "The database reached an invalid state."
      _ -> error "The database reached an invalid state."
