{-# OPTIONS_GHC -F -pgmF=record-dot-preprocessor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Backtest.App.Counter where

import Backtest.Prelude
import Juniper
import Control.Monad.IO.Class (MonadIO)
import Lucid (button_)
import Web.UI hiding (pr)
import Backtest.App.Style

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
load Nothing  = pure $ Model 0
load (Just pr) = pure $ Model pr.count

-- asdf
update :: MonadIO m => Action -> Model -> m Model
update Increment m = pure $ (m :: Model) { count = m.count + 1 }
update Decrement m = pure $ (m :: Model) { count = m.count - 1 }

view :: Model -> Html ()
view m = row (gap S0 . h Full) $ do

    button Decrement (bgButton . p S4) $ "Decrement"

    col (grow . bg Red . items Center . justify Center) $ do
      row (bg Green . text Xl8 . w S48 . justify Center) $ toHtml $ show m.count

    button Increment (bgButton . p S4) $ "Increment"

  where
    -- really easy to reuse styles!
    bgButton = hover |: bg BlueLight . bg Blue . text White


page :: MonadIO m => Page Params Model Action m
page = Page params load update view
