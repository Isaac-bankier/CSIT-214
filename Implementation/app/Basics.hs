module Basics where

import Database.SQLite.Simple
import Web.Spock
import Db

type Customer = User
type Employee = User
data ActingCustomer = ActingCustomer Employee Customer

data UserMode = IsCustomer Customer
              | IsEmployee Employee
              | IsActingCustomer ActingCustomer
              
type SessionVal = Maybe UserMode
type Server ctx = SpockCtxM ctx Connection SessionVal () ()
type Handler ctx a = ActionCtxT ctx (WebStateM Connection SessionVal ()) a
