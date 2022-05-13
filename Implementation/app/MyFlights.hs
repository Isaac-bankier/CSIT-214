{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module MyFlights ( myFlights ) where

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

myFlights :: Handler (HVect xs) a
myFlights = maybeUser $ \case
  Just u -> mkSite $ scaffold $ do
    flights <- lift $ runSqlQuery "SELECT * FROM bookings WHERE user = ?" [_userID u]
    case flights of
      [] -> h1_ "You haven't booked any flights."
      _ -> table_ $ foldr (*>) (return ()) $ fmap displayBooking flights
  Nothing -> redirect "/login"

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
            td_ $ form_ [method_ "get"] $ do
              input_ [type_ "hidden", name_ "bookingId", value_ $ T.pack $ show $ _bookingID b]
              input_ [type_ "submit", value_ "Cancel Booking"]
          _ -> error "The database reached an invalid state."
      _ -> error "The database reached an invalid state."
