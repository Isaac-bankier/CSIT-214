{-# LANGUAGE TypeFamilies #-}

module Util where

import Database.SQLite.Simple
import Web.Spock

runSql :: (HasSpock m, SpockConn m ~ Connection, ToRow q, FromRow r) => Query -> q -> m [r]
runSql q d = runQuery (\conn -> query conn q d)
