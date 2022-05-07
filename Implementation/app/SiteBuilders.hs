module SiteBuilders where

import Basics
import qualified Data.Text as T
import Lucid
import qualified Network.HTTP.Types as Http
import Web.Spock
import Web.Spock.Lucid

mkSite :: Html a -> Handler ctx b
mkSite = lucid

noAccessPage :: T.Text -> Handler ctx a
noAccessPage msg = do
  setStatus Http.status403
  mkSite (toHtml msg)
