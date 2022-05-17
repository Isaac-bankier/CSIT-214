{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module EmployeeAct ( employeeAct ) where

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

employeeAct :: Server (HVect xs)
employeeAct = do
  prehook employeeHook $ get "/actAsCustomer" actAsCustomer
  prehook employeeHook $ post "/actAsCustomer" $ do
    e <- param' "email"
    switchToCustomer e

actAsCustomer :: Handler (HVect (Employee ': xs)) a
actAsCustomer = mkSite $ scaffold $ do
  h1_ "Please enter a customer email"
  form_ [method_ "post", action_ "/bookServices"] $ do
    input_ [type_ "email", name_ "email"]
    input_ [type_ "submit", value_ "Act As Customer"]

switchToCustomer :: Int -> Handler (HVect (Employee ': xs)) a
switchToCustomer uid = do
  userQ <- runSqlQuery "SELECT * FROM users WHERE email = ?" [uid]
  e <- H.head <$> getContext
  case userQ of
    [c@(User _ _ _ _ "customer")] -> do
      sessionRegenerateId
      writeSession $ Just $ IsActingCustomer $ ActingCustomer e c
      redirect "/"
    _ -> redirect "/actAsCustomer"
