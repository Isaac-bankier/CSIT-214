{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Css
  ( css
  ) where

import Basics
import Data.HVect
import Web.Spock hiding (root, body)
import Clay
import qualified Data.Text.Lazy as T
import Prelude hiding ((**), rem, div)

css :: Server (HVect xs)
css = do
  get "/styles.css" styles

styles :: Handler ctx a
styles = do
  text $ T.toStrict $ render $ do
    mainStyles
    loginStyles
    scaffoldStyles
    tableStyles

mainStyles :: Css
mainStyles = do
  body ? do
    importUrl "'https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400&display=swap'"
    fontFamily ["Open Sans"] [sansSerif]
    padding (px 0) (px 0) (px 0) (px 0)
    margin (px 0) (px 0) (px 0) (px 0)

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
    boxShadow [bsColor "#00000059" (shadowWithBlur (px 0) (px 5) (px 15))]
  div # "#login-box" |> div ? do
    width (pct 100)
    padding (rem 3) (rem 3) (rem 2) (rem 3)
    display flex
    flexDirection column
    textAlign center
  div # "#login-box" ** h1 # ".failed" ? do
    color "#c91718"
  div # "#login-box" |> div |> h1 ? do
    marginBottom $ rem 3
    paddingBottom $ rem 1
    borderBottom (px 2) solid "#d3d3d3"
  div # "#login-box" |> div # firstChild ? do
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
    boxShadow [none]

scaffoldStyles :: Css
scaffoldStyles = do
  nav ? do
    width $ vw 100
    height $ vh 7
    lineHeight $ vh 7
    position fixed
    top $ px 0
    zIndex 2
    backgroundColor "#c91718"
    boxShadow [bsColor "#00000030" (shadowWithBlur (px 0) (px 10) (px 20)), bsColor "#0000003b" (shadowWithBlur (px 0) (px 6) (px 6))]
  nav |> a ? do
    color "#fff"
    fontSize $ rem 1.4
    marginLeft $ rem 1.2
    float floatLeft
    textDecoration none
  main_ ? do
    marginTop $ vh 8
    paddingLeft $ rem 1

tableStyles :: Css
tableStyles = do
  table ? do
    marginLeft auto
    marginRight auto
    "table-layout" -: "auto"
    width $ pct 70
    borderCollapse collapse
    borderRadius (px 8)  (px 8) (px 8) (px 8)
    boxShadow [bsColor "#00000030" (shadowWithBlur (px 0) (px 10) (px 20)), bsColor "#0000003b" (shadowWithBlur (px 0) (px 6) (px 6))]
  tbody ? do
    borderRadius (px 8)  (px 8) (px 8) (px 8)
  tr ? do
    backgroundColor "#f2f2f2"
    transition "background" 0.4 easeInOut 0
    minHeight $ rem 1
  tr # nthChild "even" ? do
    backgroundColor "#eaeaea"
  tr # firstChild ? do
    backgroundColor "#424250"
    color "#fff"
  tr # hover ? do
    backgroundColor "#dddddd"
  td ? do
    paddingLeft $ rem 1
