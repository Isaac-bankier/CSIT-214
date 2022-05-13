{-# LANGUAGE TypeFamilies #-}

module Util where

import Database.SQLite.Simple
import Web.Spock

runSqlQuery :: (HasSpock m, SpockConn m ~ Connection, ToRow q, FromRow r) => Query -> q -> m [r]
runSqlQuery q d = runQuery (\conn -> query conn q d)

runSqlStmt :: (HasSpock m, SpockConn m ~ Connection, ToRow q) => Query -> q -> m ()
runSqlStmt q d = runQuery (\conn -> execute conn q d)
