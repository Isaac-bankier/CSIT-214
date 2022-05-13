{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Login
  ( login
  , logout
  ) where

import Basics
import Control.Applicative
import Data.HVect
import qualified Data.Text as T
import Db
import Lucid
import Network.HTTP.Types (methodGet)
import Network.Wai
import SiteBuilders
import Util
import Web.Spock

login :: Handler (HVect xs) a
login = do
  method <- requestMethod <$> request
  if method == methodGet
    then mkSite $ do
    p_ "Hello please login"
    form_ [method_ "post"] $ do
      input_ [type_ "email", name_ "username"]
      input_ [type_ "password", name_ "password"]
      input_ [type_ "submit", value_ "Login"]
    else do
    u <- param "username" :: Handler (HVect xs) (Maybe T.Text)
    p <- param "password" :: Handler (HVect xs) (Maybe T.Text)
    case liftA2 (,) u p  of
      Just (u', p') -> do
        userQ <- runSqlQuery "SELECT * FROM users WHERE email = ? AND password = ?" [u', p']
        case userQ of
          [User {}] -> do
            sessionRegenerateId
            writeSession $ Just 0
            redirect "/"
          _ -> redirect "/login"
      Nothing -> redirect "/login"

logout :: Handler (HVect xs) a
logout = do
  writeSession Nothing
  redirect "/"
