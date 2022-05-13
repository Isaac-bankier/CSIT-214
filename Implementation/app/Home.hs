{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Home
  ( home
  ) where

import Basics
import Data.HVect
import Lucid
import SiteBuilders

home :: Handler (HVect xs) a
home = mkSite $ scaffold $ do
  p_ "Hello World"
