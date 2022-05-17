{-# LANGUAGE OverloadedStrings #-}

module Css
  ( css
  ) where

import Basics
import Data.HVect
import Db
import Lucid
import SiteBuilders
import qualified Data.HVect as H
import Web.Spock hiding (body)
import qualified Data.Text as T
import Clay
import qualified Data.Text.Lazy as T

css :: Server (HVect xs)
css = do
  get "/styles.css" styles

styles :: Handler ctx a
styles = do
  text $ T.toStrict $ render $ body ? background red
