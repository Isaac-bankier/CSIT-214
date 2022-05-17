{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Home
  ( home
  ) where

import Basics
import Data.HVect
import Db
import Lucid
import SiteBuilders
import qualified Data.HVect as H
import Web.Spock
import qualified Data.Text as T

home :: Handler (HVect (UserMode ': xs)) a
home = do
  u <- H.head <$> getContext
  scaffold $ 
    case u of
      IsCustomer u' ->  p_ $ toHtml $ T.concat ["Hello, ", _name u']
      IsEmployee u' ->  p_ $ toHtml $ T.concat ["Hello, ", _name u']
      IsActingCustomer (ActingCustomer _ u') ->  p_ $ toHtml $ T.concat ["Hello, ", _name u']
