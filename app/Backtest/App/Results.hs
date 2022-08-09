{-# LANGUAGE DeriveAnyClass #-}
module Backtest.App.Results where

import Backtest.Types (USD, Fund(Bal), Total, Allocation(..), usd)
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
view m = col (gap S0) $ do

  row (gap S0) $ do
    "Age: " 
    (toHtml $ show m.state.age)

  row (gap S4 . p S4) $ do
    button (Age 50) (bgButton . p S4) $ "Set Age 50"
    button (Age 60) (bgButton . p S4) $ "Set Age 60"

    -- col (grow . bg Red . items Center . justify Center) $ do
    --   row (bg Green . text Xl8 . w S48 . justify Center) $ toHtml $ show m.count

    -- button Increment (bgButton . p S4) $ "Increment"

  where
    -- really easy to reuse styles!
    bgButton = hover |: bg BlueLight . bg Blue . text White


page :: MonadIO m => Page State Model Action m
page = Page params load update view
