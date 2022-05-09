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
  , withdrawal :: USD Amt Withdrawal
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
  , withdrawals :: WithdrawalResults
  } deriving (Show)

data Success

data RateResult = RateResult
  { rate :: Pct Withdrawal
  , success :: Pct Success
  , results :: [SimResult]
  , avgEndPortfolio :: USD Bal Total
  , medEndPortfolio :: USD Bal Total
  } deriving (Show)


-- I want to know HOW MANY lows we have?
-- how many below...
-- a histogram of returns relative to initial
data WithdrawalResults = WithdrawalResults
  { init :: USD Amt Withdrawal
  , low :: USD Amt Withdrawal
  , p10 :: USD Amt Withdrawal
  , p25 :: USD Amt Withdrawal
  , med :: USD Amt Withdrawal
  , p75 :: USD Amt Withdrawal
  , p90 :: USD Amt Withdrawal
  } deriving (Show)
