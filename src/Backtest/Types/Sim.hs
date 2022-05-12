{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Types.Sim where

import Backtest.Prelude
import Backtest.Types.Usd
import Backtest.Types.Pct
import Backtest.Types.History
import Backtest.Types.Portfolio




-- Where you are at the end of the given year, before taking any action
data YearStart = YearStart
  { year       :: Year
  , history    :: Maybe History
  , start      :: Balances
  , end        :: Balances
  , returns    :: Changes
  , withdrawal :: USD Amt Withdrawal
  , actions    :: Changes
  } deriving (Show)

data SimResult = SimResult
  { startYear :: Year
  , startBalance :: Balances
  , endYear :: Year
  , endBalance :: Balances
  , years :: [YearStart]
  , wdAmts :: WithdrawalResults
  , wdSpread :: WithdrawalSpread
  } deriving (Show)

data Success

data RateResult = RateResult
  { rate :: Pct Withdrawal
  , success :: Pct Success
  , results :: [SimResult]
  , avgEndPortfolio :: USD Bal Total
  , medEndPortfolio :: USD Bal Total
  } deriving (Show)

data AggregateWithdrawals = AggregateWithdrawals
  { totalSpread :: WithdrawalSpread -- ^ add them all together
  , worstSpread :: WithdrawalSpread -- ^ the worst year
  , numSamples  :: WithdrawalSpread -- ^ number of samples that have at least one of each
  } deriving Show

-- how do you decide? Oh, below median
data WithdrawalResults = WithdrawalResults
  { init :: USD Amt Withdrawal
  , low :: USD Amt Withdrawal
  , p10 :: USD Amt Withdrawal
  , p25 :: USD Amt Withdrawal
  , med :: USD Amt Withdrawal
  , p75 :: USD Amt Withdrawal
  , p90 :: USD Amt Withdrawal
  } deriving (Show)

data WithdrawalSpread = WithdrawalSpread
  { wlow :: Int
  , w2_0 :: Int
  , w2_5 :: Int
  , w3_0 :: Int
  , w3_5 :: Int
  , w4_0 :: Int
  , w4_5 :: Int
  , whigh :: Int
  } deriving (Show)

