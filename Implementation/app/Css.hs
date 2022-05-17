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
import Web.Spock hiding (root, body)
import qualified Data.Text as T
import Clay
import qualified Data.Text.Lazy as T
import Prelude hiding (rem, div)

css :: Server (HVect xs)
css = do
  get "/styles.css" styles

styles :: Handler ctx a
styles = do
  text $ T.toStrict $ render $ do
    mainStyles
    loginStyles

mainStyles :: Css
mainStyles = do
  body ? do
    importUrl "'https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400&display=swap'"
    fontFamily ["Open Sans"] [sansSerif]

loginStyles :: Css
loginStyles = do
  div # "#login-box" ? do
    fontWeight lighter
    width $ pct 40
    height $ pct 60
    marginLeft auto
    marginRight auto
    marginTop $ pct 7
    display flex
    flexDirection row
    border (px 1) solid "#d3d3d3"
    borderRadius (px 8) (px 8) (px 8) (px 8)
    boxShadow $ pure $ bsColor "#00000059" (shadowWithBlur (px 0) (px 5) (px 15))
  div # "#login-box" |> div ? do
    width (pct 100)
    padding (rem 3) (rem 3) (rem 2) (rem 3)
    display flex
    flexDirection column
    textAlign center
  div # "#login-box" |> div |> h1 ? do
    marginBottom $ rem 3
    paddingBottom $ rem 1
    borderBottom (px 2) solid "#d3d3d3"
  div # "#login-box" |> div # ":first-child" ? do
    content $ stringContent ""
    borderRight (px 2) solid "#d3d3d3"
    alignSelf  stretch
  div # "#login-box" |> div |> form ? do
    display flex
    flexDirection column
    alignItems center
    justifyContent spaceEvenly
  div # "#login-box" |> div |> form |> star ? do
    display block
    marginBottom $ rem 1.2
    fontSize $ rem 1
    padding (rem 0.4) (rem 0.4) (rem 0.4) (rem 0.4)
    border (px 0) none "#000000"
    borderBottom (px 1) solid "#d3d3d3"
    boxShadow $ pure none

