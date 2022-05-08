module Backtest.Types.Sim where

import Backtest.Prelude
import Backtest.Types.Usd
import Backtest.Types.Pct
import Backtest.Types.History
import Backtest.Types.Portfolio



data YearResult = YearResult
  { history    :: History
  , start      :: Balances
  , end        :: Balances
  , returns    :: Changes
  , withdrawals :: USD Amt Withdrawal
  , actions    :: Changes
  -- , rebalance :: Changes
  -- , cpi        :: Pct Inflation
  } deriving (Show)

data SimResult = SimResult
  { startYear :: Year
  , startBalance :: Balances
  , endYear :: Year
  , endBalance :: Balances
  , years :: [YearResult]
  } deriving (Show)

data Success

data RateResult = RateResult
  { years :: Int
  , rate :: Pct Withdrawal
  , success :: Pct Success
  , results :: [SimResult]
  , avgPortfolio :: USD Bal Total
  , medPortfolio :: USD Bal Total
  } deriving (Show)

