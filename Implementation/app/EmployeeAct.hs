{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module EmployeeAct ( employeeAct ) where

import Basics
import Data.HVect hiding (singleton)
import Db
import Lucid
import SiteBuilders
import Util
import Web.Spock
import UserManagement
import qualified Data.HVect as H
import qualified Data.Text as T

employeeAct :: Server (HVect xs)
employeeAct = do
  prehook employeeHook $ get "/actAsCustomer" actAsCustomer
  prehook employeeHook $ post "/actAsCustomer" $ do
    e <- param' "email"
    switchToCustomer e

actAsCustomer :: Handler (HVect (Employee ': xs)) a
actAsCustomer = employeeScaffold $ do
  h1_ "Please enter a customer email"
  form_ [method_ "post", action_ "/actAsCustomer"] $ do
    input_ [type_ "email", name_ "email"]
    input_ [type_ "submit", value_ "Act As Customer"]

switchToCustomer :: T.Text -> Handler (HVect (Employee ': xs)) a
switchToCustomer uid = do
  userQ <- runSqlQuery "SELECT * FROM users WHERE email = ? AND role = \"customer\"" [uid]
  e <- H.head <$> getContext
  case userQ of
    [c@User {}] -> do
      writeSession $ Just $ IsActingCustomer $ ActingCustomer e c
      redirect "/"
    _ -> redirect "/actAsCustomer"
