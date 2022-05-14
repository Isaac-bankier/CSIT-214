{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module UserManagement where

import Basics
import Control.Monad (liftM)
import Data.HVect
import SiteBuilders
import Web.Spock
import Db
import Util

baseHook :: Handler () (HVect '[])
baseHook = return HNil

authHook :: Handler (HVect xs) (HVect (User ': xs))
authHook = maybeUser $ \mUser -> do
  oldCtx <- getContext
  case mUser of
    Nothing -> redirect "/login"
    Just val -> return (val :&: oldCtx)

loggedOutHook :: Handler (HVect xs) (HVect (LoggedOut ': xs))
loggedOutHook =
    maybeUser $ \mUser ->
    do oldCtx <- getContext
       case mUser of
         Nothing -> return (LoggedOut :&: oldCtx)
         Just _ -> redirect "/"

maybeUser :: (Maybe User -> Handler ctx a) -> Handler ctx a
maybeUser action = do
  sess <- readSession
  case sess of
    Nothing -> action Nothing
    Just sid -> do
      userQ <- runSqlQuery "SELECT * FROM users WHERE id = ?" [sid]
      case userQ of
        [u@User {}] -> do
          action $ Just u
        _ -> action Nothing

data LoggedOut = LoggedOut
