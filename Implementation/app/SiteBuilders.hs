{-# LANGUAGE OverloadedStrings #-}

module SiteBuilders where

import Basics
import qualified Data.Text as T
import Lucid
import qualified Network.HTTP.Types as Http
import Web.Spock
import Web.Spock.Lucid
import Database.SQLite.Simple

mkSite :: HtmlT (WebStateM Connection SessionVal ()) a -> Handler ctx b
mkSite = lucidT

noAccessPage :: T.Text -> Handler ctx a
noAccessPage msg = do
  setStatus Http.status403
  mkSite (toHtml msg)

scaffold :: Monad m => HtmlT m () -> HtmlT m ()
scaffold main = do
  nav_ $ do
    a_ [href_ "/"] "Home"
    a_ [href_ "/myFlights"] "My Flights"
    a_ [href_ "/bookFlights"] "Book Flights"
    a_ [href_ "/bookServices"] "Book Services"
    a_ [href_ "/logout"] "Logout"
  main_ $ do
    main
