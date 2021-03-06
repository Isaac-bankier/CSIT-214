{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Home
  ( home
  ) where

import Basics
import Data.HVect
import Db
import Lucid
import SiteBuilders
import qualified Data.HVect as H
import Web.Spock
import qualified Data.Text as T

home :: Handler (HVect (UserMode ': xs)) a
home = do
  u <- H.head <$> getContext
  case u of
    IsCustomer _ -> redirect "/myFlights"
    IsEmployee _ -> redirect "/actAsCustomer"
    IsActingCustomer _ -> redirect "/myFlights"
