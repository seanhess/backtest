{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Backtest.App.Results where

import Backtest.Types (USD, Fund(Bal), Total, Allocation(..), usd, fromAlloc, History, showPct)
import Backtest.Types.Usd (dumpUsd, dollars)
import Backtest.Prelude
import Text.Read (readMaybe)
import Juniper
import Control.Monad.IO.Class (MonadIO)
import Lucid (button_)
import Web.UI hiding (pr)
import Backtest.App.Style


newtype Alloc = Alloc { toAllocation :: Allocation }
  deriving newtype (ToParam, Show, Read, Eq, Enum, Bounded)

instance Value Alloc where
  empty = Alloc S100

data Model = Model
  { lastInputs :: Inputs
  , inputs :: Inputs
  , histories :: NonEmpty History
  } deriving (Read, Show, Encode LiveModel)

data Inputs = Inputs
  { age :: Int
  , investments :: USD (Bal Total)
  , allocation :: Alloc
  } deriving (Eq, Read, Show, Generic, ToParams)

data Action
  = SetAge Text
  | SetPortfolio Text
  | SetAlloc Alloc
  | Calculate
  deriving (Show, Read, Encode LiveAction)

params :: Model -> Inputs
params = (.inputs)

load :: MonadIO m => NonEmpty History -> Maybe Inputs -> m Model
load hs Nothing  = pure $ Model 
  { lastInputs = defaultInputs
  , inputs = defaultInputs
  , histories = hs
  }
  where
    defaultInputs = Inputs
      { age = 60
      , investments = usd 1000000
      , allocation = Alloc S70
      }
load hs (Just i) = pure $ Model
  { lastInputs = i
  , inputs = i
  , histories = hs
  }

-- asdf
update :: MonadIO m => Action -> Model -> m Model
update (SetAge t) m = do
  let age = fromMaybe m.inputs.age $ readMaybe $ cs t
  pure m { inputs = m.inputs { age = age} }

update (SetPortfolio t) m = do
  let dlr = readMaybe $ cs t :: Maybe Float
  let inv = fromMaybe m.inputs.investments $ usd <$> dlr
  pure m { inputs = m.inputs { investments = inv } }

update (SetAlloc a) m = do
  pure m { inputs = m.inputs { allocation = a }}

update (Calculate) m = do
  pure m { lastInputs = m.inputs }

view :: Model -> Html ()
view m = col (gap S1 . p S8) $ do
  let i = m.inputs

  col (gap S1) $ do
    el (text Xl . uppercase) "Safe To Spend"
    el (text Xl8) $ "$45,000"

  el (bg Red . h S72) "Graph"

  el (text Xl . uppercase) "Options"
  col (gap S1) $ do
    el' "Method"
    el' "Spend Cap"

  el (text Xl . uppercase) "My Details"
  col (gap S1) $ do
    inputs $ do
      inpLeft " Years Old" 
      inpRight $
        input SetAge (cs $ show $ i.age) (underline . placeholder "60")

    inputs $ do
      inpLeft " Portfolio" 
      inpRight $
        currencyInput SetPortfolio i.investments (underline)

    inputs $ do
      inpLeft "% Stocks / Bonds" 
      inpRight $ do
        dropdown SetAlloc (cs . showPct . fromAlloc . toAllocation) id ([minBound..maxBound] :: [Alloc])

  -- visible when changes have been made
  when (m.inputs /= m.lastInputs) $ 
    bgButton Calculate "Recalculate"

  where
    bgButton :: Encode LiveAction act => act -> Html a -> Html a
    bgButton act = button act
      ( hover |: bg BlueLight . bg Blue
      . active |: translate (X Px) . active |: translate (Y Px)
      . text White
      . p S2 . px S8
      )

    currencyInput :: Encode LiveAction act => (Text -> act) -> USD a -> Att a -> Html ()
    currencyInput act amt f =
      stack (w Full) $ do
        input act (cs $ show $ dollars amt) (left S3 . absolute' . underline)
        el (absolute' . left S0 . underline) "$"

    -- never specify only a style
    -- always surround with parens
    inputs = row (gap S2)
    inpLeft = row (w S44 . justify End)
    inpRight = el (w Full)





page :: MonadIO m => NonEmpty History -> Page Inputs Model Action m
page hs = Page params (load hs) update view
