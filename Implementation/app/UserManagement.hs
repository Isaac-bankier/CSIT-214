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

authHook :: Handler (HVect xs) (HVect ((UserId, User) ': xs))
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

maybeUser :: (Maybe (UserId, User) -> Handler ctx a) -> Handler ctx a
maybeUser action = do
  sess <- readSession
  case sess of
    Nothing -> action Nothing
    Just sid -> action $ Just (undefined, undefined)

data IsCustomer = IsCustomer

data UserId = UserId
data User = User
data LoggedOut = LoggedOut

userIsCustomer :: User -> Bool
userIsCustomer _ = True

customerHook :: ListContains n (UserId, User) xs => Handler (HVect xs) (HVect (IsCustomer ': xs))
customerHook = do
  (_ :: UserId, user) <- liftM findFirst getContext
  oldCtx <- getContext
  if userIsCustomer user then return (IsCustomer :&: oldCtx) else noAccessPage "You don't have enough rights, sorry"
