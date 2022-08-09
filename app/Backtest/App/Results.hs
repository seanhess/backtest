{-# LANGUAGE DeriveAnyClass #-}
module Backtest.App.Results where

import Backtest.Types (USD, Fund(Bal), Total, Allocation(..), usd)
import Backtest.Types.Usd (dumpUsd)
import Backtest.Prelude
import Juniper
import Control.Monad.IO.Class (MonadIO)
import Lucid (button_)
import Web.UI hiding (pr)
import Backtest.App.Style


data Model = Model
  { state :: State
  } deriving (Read, Show, ToState)

data State = State
  { age :: Int
  , investments :: USD (Bal Total)
  , allocation :: Allocation
  } deriving (Read, Show, Generic, ToParams)

data Action
  = Age Int
  deriving (Show, Read, PageAction)

params :: Model -> State
params = (.state)

load :: MonadIO m => Maybe State -> m Model
load Nothing  = pure $ Model $ State
  { age = 60
  , investments = usd 1000000
  , allocation = S70
  }
load (Just s) = pure $ Model s

-- asdf
update :: MonadIO m => Action -> Model -> m Model
update (Age i) m = pure m { state = m.state { age = i} }

view :: Model -> Html ()
view m = col (gap S1 . p S8) $ do
  let s = m.state

  col (gap S1) $ do
    el (text Xl . uppercase) "Safe To Spend"
    el (text Xl8) $ "$45,000"

  row (gap S1) $ do
    el (text Xl . uppercase) "100% Confidence"

  el (bg Red . h S72) "Graph"

  el (text Xl . uppercase) "Options"
  col (gap S1) $ do
    el' "Method"
    el' "Spend Cap"

  el (text Xl . uppercase) "My Details"
  col (gap S1) $ do
    inputs $ do
      inpLeft " Years Old" 
      inpRight (toHtml $ show s.age)

    inputs $ do
      inpLeft " Portfolio" 
      inpRight (toHtml $ dumpUsd s.investments)

    inputs $ do
      inpLeft "% Stocks / Bonds" 
      inpRight (toHtml $ drop 1 $ show $ s.allocation)

  row (gap S4) $ do
    bgButton (Age 50) $ "Set Age 50"
    bgButton (Age 60) $ "Set Age 60"

  where
    bgButton act = button act
      ( hover |: bg BlueLight . bg Blue
      . active |: translate (X Px) . active |: translate (Y Px)
      . text White
      . p S2 . px S8
      )

    -- never specify a style
    -- always surround with parens
    inputs = row (gap S2)
    inpLeft = row (w S44 . justify End)
    inpRight = el (w Full)





page :: MonadIO m => Page State Model Action m
page = Page params load update view
