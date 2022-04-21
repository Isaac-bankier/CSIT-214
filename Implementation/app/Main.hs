{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (liftM)
import Data.HVect
import qualified Data.Text as T
import Database.SQLite.Simple
import Lucid
import qualified Network.HTTP.Types as Http
import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid

type SessionVal = Maybe SessionId
type Server ctx = SpockCtxM ctx Connection SessionVal () ()
type Handler ctx a = ActionCtxT ctx (WebStateM Connection SessionVal ()) a

main :: IO ()
main = do
  cfg <- defaultSpockCfg Nothing (PCConn $ ConnBuilder (open "test.db") close (PoolCfg 1 1 1)) ()
  runSpock 8000 $ spock cfg app

runSql :: (HasSpock m, SpockConn m ~ Connection, ToRow q, FromRow r) => Query -> q -> m [r]
runSql q d = runQuery (\conn -> query conn q d)

baseHook :: Handler () (HVect '[])
baseHook = return HNil

data IsCustomer = IsCustomer

data UserId = UserId
data User = User

userIsCustomer :: User -> Bool
userIsCustomer _ = True

customerHook :: ListContains n (UserId, User) xs => Handler (HVect xs) (HVect (IsCustomer ': xs))
customerHook = do
  (_ :: UserId, user) <- liftM findFirst getContext
  oldCtx <- getContext
  if userIsCustomer user then return (IsCustomer :&: oldCtx) else noAccessPage "You don't have enough rights, sorry"

app :: Server ()
app = prehook baseHook $
  do
    get "/" $ mkSite "Hello"
    get "/about" $ mkSite "About"
    prehook authHook $ get "/authorised" $ mkSite "Authorised"

authHook :: Handler (HVect xs) (HVect ((UserId, User) ': xs))
authHook = maybeUser $ \mUser -> do
  oldCtx <- getContext
  case mUser of
    Nothing -> noAccessPage "Unknown user. Login first!"
    Just val -> return (val :&: oldCtx)

indexHandler :: Handler () ()
indexHandler = undefined

noAccessPage :: T.Text -> Handler ctx a
noAccessPage msg = do
  setStatus Http.status403
  mkSite (toHtml msg)

mkSite :: Html () -> Handler ctx a
mkSite = lucid

maybeUser :: (Maybe (UserId, User) -> Handler ctx a) -> Handler ctx a
maybeUser action = do
  sess <- readSession
  case sess of
    Nothing -> action Nothing
    Just sid -> action $ Just (undefined, undefined)
