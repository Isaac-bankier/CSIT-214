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
    buttonStyles

mainStyles :: Css
mainStyles = do
  body ? do
    importUrl "'https://fonts.googleapis.com/css2?family=Open+Sans:wght@300;400&display=swap'"
    fontFamily ["Open Sans"] [sansSerif]
    padding (px 0) (px 0) (px 0) (px 0)
    margin (px 0) (px 0) (px 0) (px 0)

loginStyles :: Css
loginStyles = do
  img # "#logo" ? do
    display block
    width $ pct 15
    marginLeft auto
    marginRight auto
  div # "#login-box" ? do
    fontWeight lighter
    width $ pct 40
    height $ pct 60
    marginLeft auto
    marginRight auto
    display flex
    flexDirection row
    border (px 1) solid "#d3d3d3"
    borderRadius (px 8) (px 8) (px 8) (px 8)
    boxShadow [bsColor "#00000030" (shadowWithBlur (px 0) (px 10) (px 20)), bsColor "#0000003b" (shadowWithBlur (px 0) (px 6) (px 6))]
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

scaffoldStyles :: Css
scaffoldStyles = do
  nav ? do
    width $ vw 100
    height $ vh 7
    lineHeight $ vh 7
    position fixed
    top $ px 0
    zIndex 2
    backgroundColor "#334e6b"
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
    paddingBottom $ vh 8

tableStyles :: Css
tableStyles = do
  table ? do
    marginLeft auto
    marginRight auto
    "table-layout" -: "auto"
    width $ pct 70
    borderCollapse collapse
    borderRadius (px 8) (px 8) (px 8) (px 8)
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
    backgroundColor "#334e6b"
    color "#fff"
  tr # hover ? do
    backgroundColor "#dddddd"
  tr # firstChild # hover ? do
    backgroundColor "#334e6b"
  td ? do
    padding (rem 1) (rem 1) (rem 1) (rem 1)
  td |> form ? do
    margin (px 0) (px 0) (px 0) (px 0)

buttonStyles :: Css
buttonStyles = do
  input # "type=submit" ? do
    backgroundColor "#334e6b"
    borderRadius (px 8) (px 8) (px 8) (px 8)
    borderStyle none
    boxSizing borderBox
    color "#FFFFFF"
    cursor pointer
    display inlineBlock
    fontSize $ px 14
    fontWeight $ weight 500
    height $ px 40
    lineHeight $ px 20
    margin (px 0) (px 0) (px 0) (px 0)
    outline solid (px 0) "#000"
    padding (rem 0.2) (rem 1) (rem 0.2) (rem 1)
    position relative
    textAlign center
    textDecoration none
    transition "background-color" 0.4 easeInOut 0
    verticalAlign vAlignBaseline
    userSelect none;
  input # "type=submit" # hover ? do
    backgroundColor "#3dc2ca"
  input # "type=submit" # focus ? do
    backgroundColor "#3dc2ca"
