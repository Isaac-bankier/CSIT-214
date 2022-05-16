{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module UserManagement where

import Basics
import Data.HVect
import Web.Spock

baseHook :: Handler () (HVect '[])
baseHook = return HNil

customerHook :: Handler (HVect xs) (HVect (Customer ': xs))
customerHook = maybeUser $ \mUser -> do
  oldCtx <- getContext
  case mUser of
    Just (IsCustomer c) -> return (c :&: oldCtx)
    Just (IsActingCustomer (ActingCustomer _ c)) -> return (c :&: oldCtx)
    _ -> redirect "/login"

employeeHook :: Handler (HVect xs) (HVect (Employee ': xs))
employeeHook = maybeUser $ \mUser -> do
  oldCtx <- getContext
  case mUser of
    Just (IsEmployee e) -> return (e :&: oldCtx)
    _ -> redirect "/login"

actingCustomerHook :: Handler (HVect xs) (HVect (ActingCustomer ': xs))
actingCustomerHook = maybeUser $ \mUser -> do
  oldCtx <- getContext
  case mUser of
    Just (IsActingCustomer c) -> return (c :&: oldCtx)
    _ -> redirect "/login"

authHook :: Handler (HVect xs) (HVect (UserMode ': xs))
authHook = maybeUser $ \mUser -> do
  oldCtx <- getContext
  case mUser of
    Nothing -> redirect "/login"
    Just u -> return (u :&: oldCtx)

data LoggedOut = LoggedOut

loggedOutHook :: Handler (HVect xs) (HVect (LoggedOut ': xs))
loggedOutHook =
    maybeUser $ \mUser ->
    do oldCtx <- getContext
       case mUser of
         Nothing -> return (LoggedOut :&: oldCtx)
         Just _ -> redirect "/"

maybeUser :: (Maybe UserMode -> Handler ctx a) -> Handler ctx a
maybeUser action = do
  sess <- readSession
  case sess of
    Nothing -> action Nothing
    Just u -> action $ Just u
