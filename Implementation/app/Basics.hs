module Basics where

import Database.SQLite.Simple
import Web.Spock

type SessionVal = Maybe Int
type Server ctx = SpockCtxM ctx Connection SessionVal () ()
type Handler ctx a = ActionCtxT ctx (WebStateM Connection SessionVal ()) a
