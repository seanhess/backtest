{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  { rate :: Pct Withdrawal
  , success :: Pct Success
  , results :: [SimResult]
  , avgEndPortfolio :: USD Bal Total
  , medEndPortfolio :: USD Bal Total
  } deriving (Show)


