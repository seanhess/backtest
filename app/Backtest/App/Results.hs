{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
module Backtest.App.Results where

import Backtest.Types
import Backtest.Types.Usd (dumpUsd, dollars)
import Backtest.History as History (loadHistories, samples)
import Backtest.Simulation as Simulation (simulation, now, rebalance, Actions)
import qualified Backtest.Aggregate as Aggregate
import qualified Backtest.Graph as Graph
import Backtest.Strategy as Strategy (staticWithdrawal, rebalanceFixed)
import Backtest.Strategy.Steps as Strategy (withdrawFloor)
import Backtest.Optimize as Optimize (maximizeRate, isSimValid, OptimizeResult)
import Backtest.Debug (printTable, Column(..), yearCols)
import Backtest.Prelude
import Text.Read (readMaybe)
import Juniper
import Control.Monad.IO.Class (MonadIO)
import Lucid (button_, id_)
import Web.UI hiding (pr)
import Backtest.App.Style
import Debug.Trace (traceM)
import Data.List as List (find)
import qualified Data.List.NonEmpty as NE


newtype Alloc = Alloc { toAllocation :: Allocation }
  deriving newtype (ToParam, Show, Read, Eq, Enum, Bounded)

instance Value Alloc where
  empty = Alloc S100

data Model = Model
  { lastInputs :: Inputs
  , inputs :: Inputs
  , results :: Maybe Results
  } deriving (Read, Show, Encode LiveModel)

data Inputs = Inputs
  { age :: Int
  , investments :: USD (Bal Stocks)
  , allocation :: Alloc
  } deriving (Eq, Read, Show, Generic, ToParams)

data Results = Results
  { swr :: Pct Withdrawal
  , swa :: USD (Amt Withdrawal)
  , simResults :: NonEmpty SimResult
  } deriving (Show, Read, Eq)

data Action
  = SetAge Text
  | SetPortfolio Text
  | SetAlloc Alloc
  | Calculate
  deriving (Show, Read, Encode LiveAction)

-- this is the params
-- everything else needs to be calculated
params :: Model -> Inputs
params = (.inputs)

-- we need to load it dynamically
-- well. In Elm, we would ALSO initiate a deferred update
-- that could be something else, like....
load :: MonadIO m => Maybe Inputs -> m Model
load Nothing  = do
  -- TODO "defer" : tells the client to call us back and wait
  -- defer task Command
  pure $ Model 
    { lastInputs = defaultInputs
    , inputs = defaultInputs

    -- TODO calculate results
    , results = Nothing
    }
  where
    defaultInputs = Inputs
      { age = 60
      , investments = usd 1000000
      , allocation = Alloc S70
      }
load (Just i) = pure $ Model
  { lastInputs = i
  , inputs = i

  -- TODO calculate results
  , results = Nothing
  }

-- asdf
update :: (MonadIO m, MonadFail m) => Action -> Model -> m Model
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
  hs <- liftIO $ loadHistories
  let opr = runSimulation hs m.inputs
  -- putStrLn $ (showPct . (.swr) <$> res :: Maybe (Pct Withdrawal))
  pure m { lastInputs = m.inputs, results = toResults <$> opr }
  where
    toResults opr = Results
      { swr = opr.swr
      , swa = amount opr.swr m.inputs.investments
      , simResults = opr.results
      }


view :: Model -> Html ()
view m = col (gap S1 . p S8) $ do
  let i = m.inputs

  case m.results of
    Just res -> results res
    Nothing -> loading

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
        dropdown SetAlloc m.inputs.allocation id
          (cs . showPct . fromAlloc . toAllocation)
          ([minBound..maxBound] :: [Alloc])

  when (m.inputs /= m.lastInputs || m.results == Nothing) $ 
    bgButton Calculate "Calculate"

  where

    results :: Results -> Html ()
    results r = do
      -- let gr = Graph.testGraph $ Graph.simData (usd 600) r.simResults
      col (gap S1) $ do
        el (text Xl . uppercase) "Safe To Spend"
        el (text Sm) $ toHtml $ showPct $ r.swr
        el (text Xl8) $ toHtml $ "$" <> show (dollars r.swa)

      el (bg Red . h S72 . addAttribute (id_ "test")) $ Graph.testGraph


    loading :: Html ()
    loading = el (text Xl) "Loading"

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






page :: (MonadIO m, MonadFail m) => Page Inputs Model Action m
page = Page params load update view



runSimulation :: NonEmpty History -> Inputs -> Maybe OptimizeResult
runSimulation hs inp =
  let years = numYears (90 - inp.age)
      ss = History.samples years hs
      start = Portfolio inp.investments (usd 0)
      alloc = toAllocation inp.allocation

  in lastMay $ Optimize.maximizeRate isSimValid (sims ss alloc start)

  where

    sims :: NonEmpty (NonEmpty History) -> Allocation -> Balances -> Pct Withdrawal -> NonEmpty SimResult
    sims ss alloc start wr = fmap (sim alloc start wr) ss

    sim :: Allocation -> Balances -> Pct Withdrawal -> NonEmpty History -> SimResult
    sim alloc start wr = Simulation.simulation start (actions start alloc wr)

    isYear :: Int -> SimResult -> Bool
    isYear y sr =
        sr.startYear == Year y

    actions :: Balances -> Allocation -> Pct Withdrawal -> Actions ()
    actions st al wr = do
      let swda = Strategy.staticWithdrawal wr st
      n <- Simulation.now
      Strategy.withdrawFloor swda wr
      Simulation.rebalance $ rebalanceFixed al

    -- columns :: [Column OptimizeResult]
    -- columns =
    --     [ Column "stocks%" 9 (\o -> show o.alloc)
    --     , Column "swr" 7 (\o -> show o.swr)
    --     , Column "min" 7 (\o -> show $ Aggregate.minWithdrawal o.results)
    --     , Column "med" 7 (\o -> show $ Aggregate.medWithdrawal o.results)
    --     ]

