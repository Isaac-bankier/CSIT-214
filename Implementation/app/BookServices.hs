{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module BookServices ( bookService ) where

import Basics
import Control.Applicative
import Control.Monad.Trans
import Data.HVect hiding (singleton)
import qualified Data.Text as T
import Db
import Lucid
import Network.HTTP.Types (methodGet)
import Network.Wai
import SiteBuilders
import Util
import Web.Spock
import UserManagement
import Database.SQLite.Simple

bookService :: Server (HVect xs)
bookService = do
  prehook authHook $ get "/bookServices" findItem
  prehook authHook $ get ("/bookServices" <//> var) $ \s -> findBooking s
  prehook authHook $ post "/bookServices" $ do
    sid <- param' "serviceId"
    bid <- param' "bookingId"
    makeServiceBooking sid bid

findItem :: Handler (HVect xs) a
findItem = mkSite $ scaffold $ do
  services <- lift $ runSqlQuery_ "SELECT * FROM services"
  h1_ "Please select an item to book"
  table_ $ foldr (*>) (return ()) $ fmap displayService services

findBooking :: Int -> Handler (HVect xs) a
findBooking sid = maybeUser $ \case
  Just u -> mkSite $ scaffold $ do
    bookings <- lift $ runSqlQuery "SELECT * FROM bookings WHERE user = ?" [_userID u]
    h1_ "Please select a seat to book"
    table_ $ foldr (*>) (return ()) $ fmap (displayBooking sid) bookings
  Nothing -> redirect "/login"

makeServiceBooking :: Int -> Int -> Handler (HVect xs) a
makeServiceBooking sid bid = do
  runSqlStmt "INSERT INTO service_bookings (booking, service) VALUES (?, ?)" (bid, sid)
  redirect "/myFlights"

displayService :: Monad m => Service -> HtmlT m ()
displayService s = do
  tr_ $ do
    td_ $ toHtml $ _serviceName s
    td_ $ toHtml $ _description s
    td_ $ toHtml $ show $ _serviceCost s
    td_ $ form_ [method_ "get", action_ $ T.concat ["/bookServices/", T.pack $ show $ _serviceID s]] $ do
      input_ [type_ "submit", value_ "Select Seat"]

displayBooking :: (Monad m, HasSpock m, SpockConn m ~ Connection) => Int -> Booking -> HtmlT m ()
displayBooking sid b = do
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
            td_ $ form_ [method_ "post", action_ "/bookServices"] $ do
              input_ [type_ "hidden", name_ "serviceId", value_ $ T.pack $ show sid]
              input_ [type_ "hidden", name_ "bookingId", value_ $ T.pack $ show $ _bookingID b]
              input_ [type_ "submit", value_ "Order Service"]
          _ -> error "The database reached an invalid state."
      _ -> error "The database reached an invalid state."
