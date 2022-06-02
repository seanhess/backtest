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
  , withdrawal :: USD (Amt Withdrawal)
  , netIncome  :: USD (Amt Income)
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

data AggregateWithdrawals = AggregateWithdrawals
  { totalSpread :: WithdrawalSpread Int -- ^ add them all together
  , worstSpread :: WithdrawalSpread Int -- ^ the worst year
  , numSamples  :: WithdrawalSpread (Pct Success) -- ^ number of samples that have at least one of each
  }

-- how do you decide? Oh, below median
data WithdrawalResults = WithdrawalResults
  { init :: USD (Amt Withdrawal)
  , low :: USD (Amt Withdrawal)
  , p10 :: USD (Amt Withdrawal)
  , p25 :: USD (Amt Withdrawal)
  , med :: USD (Amt Withdrawal)
  , p75 :: USD (Amt Withdrawal)
  , p90 :: USD (Amt Withdrawal)
  } deriving (Show)

data WithdrawalSpread a = WithdrawalSpread
  { wlow :: a
  , w2_0 :: a
  , w2_5 :: a
  , w3_0 :: a
  , w3_5 :: a
  , w4_0 :: a
  , w4_5 :: a
  , whigh :: a
  }

