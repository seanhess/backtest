{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Backtest.App.Counter where

import Prelude
import Juniper
import Control.Monad.IO.Class (MonadIO)
import Lucid (Html, toHtml)
import Lucid.Html5

data Model = Model
  { count :: Int
  } deriving (Read, Show, ToState)

data Params = Params
  { count :: Int
  } deriving (Generic, ToParams)

data Action
  = Increment
  | Decrement
  deriving (Show, Read, PageAction)

params :: Model -> Params
params m = Params m.count

load :: MonadIO m => Maybe Params -> m Model
load Nothing = pure $ Model 0
load (Just p) = pure $ Model p.count

update :: MonadIO m => Action -> Model -> m Model
update Increment m = pure $ (m :: Model) { count = m.count + 1 }
update Decrement m = pure $ (m :: Model) { count = m.count - 1 }

view :: Model -> Html ()
view m = section_ [ class_ "page" ] $ do

    div_ $ toHtml $ show m.count

    div_ [ class_ "section" ] $ do
      button_ [ onClick Increment] "Increment"
      button_ [ onClick Decrement] "Decrement"


page :: MonadIO m => Page Params Model Action m
page = Page params load update view
