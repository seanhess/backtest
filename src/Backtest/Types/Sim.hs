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
  , history    :: History
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
  , years :: NonEmpty YearStart
  }

data Success

data RateResult = RateResult
  { rate :: Pct Withdrawal
  , success :: Pct Success
  , results :: NonEmpty SimResult
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
  { low :: a
  , p10 :: a
  , p25 :: a
  , med :: a
  , p75 :: a
  , p90 :: a
  }

instance Show a => Show (Histogram a) where
  show h = show (h.low, h.p10, h.p25, h.med, h.p75, h.p90)


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




newtype Sorted a = Sorted { toList :: NonEmpty a }
  deriving (Foldable, Show, Eq)

sorted :: Ord a => NonEmpty a -> Sorted a
sorted as = Sorted $ sort as

minSorted :: Ord a => Sorted a -> a
minSorted as = head as.toList
