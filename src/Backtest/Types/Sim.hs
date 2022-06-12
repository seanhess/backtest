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
  , yearIndex  :: Int
  , history    :: Maybe History
  , start      :: Balances
  , end        :: Balances
  , returns    :: Changes
  , withdrawal :: USD (Amt Withdrawal)
  , netIncome  :: USD (Amt Income)
  , netExpenses :: USD (Amt Expense)
  , actions    :: Changes
  } deriving (Show)

data SimResult = SimResult
  { startYear :: Year
  , startBalance :: Balances
  , endYear :: Year
  , endBalance :: Balances
  , years :: [YearStart]
  , wdAmts :: WithdrawalResults
  , wdSpread :: WithdrawalSpread Int
  }

data Success

data RateResult = RateResult
  { rate :: Pct Withdrawal
  , success :: Pct Success
  , results :: [SimResult]
  , avgEndPortfolio :: USD (Bal Total)
  , medEndPortfolio :: USD (Bal Total)
  }

data AggregateSpread = AggregateSpread
  { totalSpread :: WithdrawalSpread Int -- ^ add them all together
  , worstSpread :: WithdrawalSpread Int -- ^ the worst year
  , numSamples  :: WithdrawalSpread (Pct Success) -- ^ number of samples that have at least one of each
  }


data MedianWithdrawal = MedianWithdrawal
  { yearIndex :: Int
  , withdrawal :: USD (Amt Withdrawal)
  , netExpenses :: USD (Amt Expense)
  }


type WithdrawalResults = Histogram (USD (Amt Withdrawal))

-- how do you decide? Oh, below median
data Histogram a = Histogram
  { init :: a
  , low :: a
  , p10 :: a
  , p25 :: a
  , med :: a
  , p75 :: a
  , p90 :: a
  }


data WithdrawalSpread a = WithdrawalSpread
  { wlow :: a
  , w2_0 :: a
  , w2_5 :: a
  , w3_0 :: a
  , w3_5 :: a
  , w4_0 :: a
  , w4_5 :: a
  , w5_0 :: a
  , w5_5 :: a
  , whigh :: a
  }

