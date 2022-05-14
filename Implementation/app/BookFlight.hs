{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module BookFlight ( bookFlight ) where

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

bookFlight :: Handler (HVect xs) a
bookFlight = do
  method <- requestMethod <$> request
  if method == methodGet
    then do
    flightId <- param "flightId" :: Handler (HVect xs) (Maybe Int)
    case flightId of
      Nothing -> mkSite $ scaffold $ do
        flights <- lift $ runSqlQuery_ "SELECT * FROM flights"
        h1_ "Please select a flight to book"
        table_ $ foldr (*>) (return ()) $ fmap displayFight flights
      Just fid -> mkSite $ scaffold $ do
        seats <- lift $ runSqlQuery "SELECT * FROM seats WHERE flight = ?" [fid]
        h1_ "Please select a seat to book"
        table_ $ foldr (*>) (return ()) $ fmap displaySeat seats
    else maybeUser $ \case
    Just u -> do
      sid <- param "seatId" :: Handler (HVect xs) (Maybe Int)
      case sid of
        Just sid' -> do
          runSqlStmt "INSERT INTO bookings (user, seat) VALUES (?, ?)" (_userID u, sid')
          redirect "/myFlights"
        Nothing -> redirect "/bookFlight"
    Nothing -> redirect "/login"

displayFight :: Monad m => Flight -> HtmlT m ()
displayFight f = do
  tr_ $ do
    td_ $ toHtml $ _from f
    td_ $ toHtml $ _to f
    td_ $ toHtml $ _date f
    td_ $ form_ [method_ "get"] $ do
      input_ [type_ "hidden", name_ "flightId", value_ $ T.pack $ show $ _flightID f]
      input_ [type_ "submit", value_ "Select Seat"]

displaySeat :: Monad m => Seat -> HtmlT m ()
displaySeat s = do
  tr_ $ do
    td_ $ toHtml $ _seatName s
    td_ $ toHtml $ show $ _cost s
    td_ $ form_ [method_ "post"] $ do
      input_ [type_ "hidden", name_ "seatId", value_ $ T.pack $ show $ _seatID s]
      input_ [type_ "submit", value_ "Purchase Seat"]
