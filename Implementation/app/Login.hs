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
import Control.Monad (when)

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
  div_ [id_ "login-box"] $ do
    div_ [id_ "login"] $ do
      h1_ "Login"
      when failed $ h2_ [id_ "login-failure"] "Login failed"
      form_ [method_ "post"] $ do
        input_ [type_ "email", placeholder_ "Email", name_ "username"]
        input_ [type_ "password", placeholder_ "Password", name_ "password"]
        input_ [type_ "submit", value_ "Login"]
    div_ [id_ "register"] $ do
      h1_ "Register"
      form_ [method_ "post"] $ do
        input_ [type_ "email", placeholder_ "Name", name_ "username"]
        input_ [type_ "email", placeholder_ "Email", name_ "username"]
        input_ [type_ "password", placeholder_ "Password", name_ "password"]
        input_ [type_ "password", placeholder_ "Password, Again", name_ "password"]
        input_ [type_ "submit", value_ "Register"]

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
