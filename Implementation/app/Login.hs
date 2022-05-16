{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Login ( login ) where

import Basics
import Data.HVect
import qualified Data.Text as T
import Db
import Lucid
import SiteBuilders
import Util
import Web.Spock
import UserManagement

login :: Server (HVect xs)
login = do
  prehook authHook $ get "/logout" logout
  prehook loggedOutHook $ get "/login" $ loginPage False
  prehook loggedOutHook $ get "/loginFailed" $ loginPage True
  prehook loggedOutHook $ post "/login" $ do
    u <- param' "username"
    p <- param' "password"
    loginAction u p

loginPage :: Bool -> Handler (HVect xs) a
loginPage failed = mkSite $ do
  if failed then h1_ "Login failed" else h1_ "Hello please login"
  form_ [method_ "post"] $ do
    input_ [type_ "email", name_ "username"]
    input_ [type_ "password", name_ "password"]
    input_ [type_ "submit", value_ "Login"]

loginAction :: T.Text -> T.Text -> Handler (HVect xs) a
loginAction u p = do
  userQ <- runSqlQuery "SELECT * FROM users WHERE email = ? AND password = ?" [u, p]
  case userQ of
    [u'@(User _ _ _ _ "customer")] -> do
      sessionRegenerateId
      writeSession $ Just $ IsCustomer u'
      redirect "/"
    [u'@(User _ _ _ _ "employee")] -> do
      sessionRegenerateId
      writeSession $ Just $ IsEmployee u'
      redirect "/"
    _ -> redirect "/loginFailed"

logout :: Handler (HVect xs) a
logout = do
  s <- readSession
  case s of
    Nothing -> writeSession Nothing
    Just (IsCustomer _) -> writeSession Nothing
    Just (IsEmployee _) -> writeSession Nothing
    Just (IsActingCustomer (ActingCustomer e _)) -> writeSession $ Just $ IsEmployee e
  redirect "/"
