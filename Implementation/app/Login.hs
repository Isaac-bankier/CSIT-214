{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Login
  ( login
  , logout
  ) where

import Basics
import Data.HVect
import SiteBuilders
import Web.Spock
import qualified Data.Text as T
import Lucid
import Network.HTTP.Types (methodGet)
import Network.Wai

login :: Handler (HVect xs) a
login = do
  method <- requestMethod <$> request
  if method == methodGet
    then mkSite $ do
    p_ "Hello please login"
    form_ [method_ "post"] $ do
      input_ [name_ "username"]
      input_ [type_ "password", name_ "password"]
      input_ [type_ "submit", value_ "Login"]
    else do
    u <- param "username" :: Handler (HVect xs) (Maybe T.Text)
    p <- param "password" :: Handler (HVect xs) (Maybe T.Text)
    case u of
      Just u' -> do
        sessionRegenerateId
        writeSession $ Just 0
        mkSite $ toHtml u'
      Nothing -> redirect "/login"

logout :: Handler (HVect xs) a
logout = do
  writeSession Nothing
  redirect "/"
