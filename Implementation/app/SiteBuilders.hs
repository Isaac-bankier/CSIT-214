{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module SiteBuilders where

import Basics
import qualified Data.HVect as H
import qualified Data.Text as T
import Database.SQLite.Simple
import Lucid
import qualified Network.HTTP.Types as Http
import UserManagement
import Web.Spock
import Web.Spock.Lucid
import Data.HVect

mkSite :: HtmlT (WebStateM Connection SessionVal ()) a -> Handler ctx b
mkSite = lucidT

scaffold :: HtmlT (WebStateM Connection SessionVal ()) a -> Handler (HVect (UserMode ': xs)) b
scaffold main = do
  u <- H.head <$> getContext
  case u of
    IsCustomer _ -> customerScaffold' main
    IsEmployee _ -> employeeScaffold' main
    IsActingCustomer _ -> customerScaffold' main

customerScaffold :: HtmlT (WebStateM Connection SessionVal ()) a -> Handler (HVect (Customer ': xs)) b
customerScaffold = customerScaffold'

customerScaffold' :: HtmlT (WebStateM Connection SessionVal ()) a -> Handler ctx b
customerScaffold' main = lucidT $ do
      html_ $ do
        body_ $ do
          nav_ $ do
            a_ [href_ "/"] "Home"
            a_ [href_ "/myFlights"] "My Flights"
            a_ [href_ "/bookFlights"] "Book Flights"
            a_ [href_ "/bookServices"] "Book Services"
            a_ [href_ "/logout"] "Logout"
          main_ $ do
            main

employeeScaffold :: HtmlT (WebStateM Connection SessionVal ()) a -> Handler (HVect (Employee ': xs)) b
employeeScaffold = employeeScaffold'

employeeScaffold' :: HtmlT (WebStateM Connection SessionVal ()) a -> Handler ctx b
employeeScaffold' main = lucidT $ do
      html_ $ do
        body_ $ do
          nav_ $ do
            a_ [href_ "/"] "Home"
            a_ [href_ "/actAsCustomer"] "Act As Customer"
            a_ [href_ "/logout"] "Logout"
          main_ $ do
            main
