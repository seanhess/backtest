{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Aggregate where



import Backtest.Prelude
import Backtest.Types
import Backtest.Simulation (withdrawals)
import Data.List.NonEmpty as NE (transpose, NonEmpty, filter)




medianWithdrawals :: NonEmpty SimResult -> NonEmpty MedianWithdrawal
medianWithdrawals srs = 
  fmap toMed $ transpose $ fmap (.years) srs
  where
    toMed yrs = MedianWithdrawal
      { yearIndex = (head yrs).yearIndex
      , withdrawal = median $ sorted $ fmap (.withdrawal) yrs
      , netExpenses = median $ sorted $ fmap (.netExpenses) yrs
      }




withdrawalResults :: Sorted (USD (Amt Withdrawal)) -> WithdrawalResults
withdrawalResults wds =
    Histogram
        { low = minimum wds
        , med = median wds
        , p10 = percentile 0.10 wds
        , p25 = percentile 0.25 wds
        , p75 = percentile 0.75 wds
        , p90 = percentile 0.90 wds
        }
    where


withdrawalSpread :: SimResult -> WithdrawalSpread Int
withdrawalSpread sr =
    WithdrawalSpread
        { wlow = lowWithdrawals start (pct 0.0) (pct 2.0) wds
        , w2_0 = lowWithdrawals start (pct 2.0) (pct 2.5) wds
        , w2_5 = lowWithdrawals start (pct 2.5) (pct 3.0) wds
        , w3_0 = lowWithdrawals start (pct 3.0) (pct 3.5) wds
        , w3_5 = lowWithdrawals start (pct 3.5) (pct 4.0) wds
        , w4_0 = lowWithdrawals start (pct 4.0) (pct 4.5) wds
        , w4_5 = lowWithdrawals start (pct 4.5) (pct 5.0) wds
        , w5_0 = lowWithdrawals start (pct 5.0) (pct 5.5) wds
        , w5_5 = lowWithdrawals start (pct 5.5) (pct 6.0) wds
        , whigh = lowWithdrawals start (pct 6.0) (pct 100) wds
        }
  where
    start = total sr.startBalance
    wds = fmap (.withdrawal) sr.years

lowWithdrawals :: USD (Bal Total) -> Pct Withdrawal -> Pct Withdrawal -> NonEmpty (USD (Amt Withdrawal)) -> Int
lowWithdrawals start low high wds =
    let l = amount low start
        h = amount high start
    in length $ NE.filter (\w -> l <= w && w < h) wds

minWithdrawal :: NonEmpty SimResult -> USD (Amt Withdrawal)
minWithdrawal srs = minimum $ fmap (minimum . withdrawals) srs

medWithdrawal :: NonEmpty SimResult -> USD (Amt Withdrawal)
medWithdrawal srs = median $ sorted $ fmap (median . withdrawals) srs

isWithdrawalFail :: SimResult -> Bool
isWithdrawalFail sr =
  let start = (head sr.years).withdrawal :: USD (Amt Withdrawal)
      low = minimum $ fmap (.withdrawal) sr.years :: USD (Amt Withdrawal)
  in low < start


aggregateResults :: NonEmpty SimResult -> NonEmpty WithdrawalResults
aggregateResults srs = srs
  & fmap (.years)
  & transpose
  & fmap (fmap (.withdrawal))
  & fmap (withdrawalResults . sorted)


aggregateResultsAll :: NonEmpty WithdrawalResults -> WithdrawalResults
aggregateResultsAll wrs = Histogram
  { low = minimum $ fmap (.low) wrs
  , med = median $ sorted $ fmap (.med) wrs
  , p10 = median $ sorted $ fmap (.p10) wrs
  , p25 = median $ sorted $ fmap (.p10) wrs
  , p75 = median $ sorted $ fmap (.p75) wrs
  , p90 = median $ sorted $ fmap (.p90) wrs
  }

-- aggregateMedian :: [WithdrawalResults] -> WithdrawalResults
-- aggregateMedian wrs = aggregateResults median wrs

-- aggregatePercentile :: Float -> [WithdrawalResults] -> WithdrawalResults
-- aggregatePercentile p wrs = aggregateResults (percentile p) wrs


aggregateSpread :: NonEmpty (WithdrawalSpread Int) -> AggregateSpread
aggregateSpread ws' =
  AggregateSpread
    { totalSpread = totalSpread' ws'
    , worstSpread = worstSpread' ws'
    , numSamples = numSamples' ws'
    }
  where
    totalSpread' ws =
      WithdrawalSpread
        { wlow = sum $ fmap (.wlow) ws
        , w2_0 = sum $ fmap (.w2_0) ws
        , w2_5 = sum $ fmap (.w2_5) ws
        , w3_0 = sum $ fmap (.w3_0) ws
        , w3_5 = sum $ fmap (.w3_5) ws
        , w4_0 = sum $ fmap (.w4_0) ws
        , w4_5 = sum $ fmap (.w4_5) ws
        , w5_0 = sum $ fmap (.w5_0) ws
        , w5_5 = sum $ fmap (.w5_5) ws
        , whigh = sum $ fmap (.whigh) ws
        }

    worstSpread' :: NonEmpty (WithdrawalSpread Int) -> WithdrawalSpread Int
    worstSpread' ws =
      head $ sortBy (comparing spreadPoints) ws


    numSamples' :: NonEmpty (WithdrawalSpread Int) -> WithdrawalSpread (Pct Success)
    numSamples' ws =
      let tot = length ws
      in WithdrawalSpread
        { wlow = pctSamples tot (.wlow) ws
        , w2_0 = pctSamples tot (.w2_0) ws
        , w2_5 = pctSamples tot (.w2_5) ws
        , w3_0 = pctSamples tot (.w3_0) ws
        , w3_5 = pctSamples tot (.w3_5) ws
        , w4_0 = pctSamples tot (.w4_0) ws
        , w4_5 = pctSamples tot (.w4_5) ws
        , w5_0 = pctSamples tot (.w5_0) ws
        , w5_5 = pctSamples tot (.w5_5) ws
        , whigh = pctSamples tot (.whigh) ws
        }


    pctSamples :: Int -> (WithdrawalSpread Int -> Int) -> NonEmpty (WithdrawalSpread Int) -> Pct Success
    pctSamples tot toSpread ws =
      pctFromFloat $ (fromIntegral (length (NE.filter (hasSpread toSpread) ws)) / fromIntegral tot)

-- count 15s highest, then 20s etc
spreadPoints :: WithdrawalSpread Int -> Int
spreadPoints ws = negate $ ws.wlow * 10000 + ws.w2_0 * 1000 + ws.w2_5 * 100 + ws.w3_0 * 10 + ws.w3_5

hasSpread :: (WithdrawalSpread Int -> Int) -> WithdrawalSpread Int -> Bool
hasSpread toSpread wd = (toSpread wd) /= 0

yearSpread :: NonEmpty SimResult -> WithdrawalSpread [Year]
yearSpread srs =
  WithdrawalSpread
    { wlow = years (.wlow)
    , w2_0 = years (.w2_0)
    , w2_5 = years (.w2_5)
    , w3_0 = years (.w3_0)
    , w3_5 = years (.w3_5)
    , w4_0 = years (.w4_0)
    , w4_5 = years (.w4_5)
    , w5_0 = years (.w5_0)
    , w5_5 = years (.w5_5)
    , whigh = years (.whigh)
    }
  where
    years f = map (.startYear) $ NE.filter (hasSpread f . withdrawalSpread) srs




averageEndPortfolio :: NonEmpty SimResult -> USD (Bal Total)
averageEndPortfolio srs =
    let tots = fmap (total . (.endBalance)) srs
    in fromCents $ round $ (fromIntegral $ sum $ fmap (totalCents) tots :: Float) / (fromIntegral $ length tots :: Float) :: USD (Bal Total)

-- Median END Balance, not portfolio
medianEndPortfolio :: NonEmpty SimResult -> USD (Bal Total)
medianEndPortfolio srs =
    let tots = fmap (total . (.endBalance)) srs
    in fromCents $ median $ sorted $ fmap totalCents tots

median :: (Ord a) => Sorted a -> a   
median xs = xs.toList !! mid   
 where mid = (length xs) `div` 2 

percentile :: (Ord a) => Float -> Sorted a -> a   
percentile p xs = xs.toList !! n
 where n = round $ (fromIntegral $ length xs) * p





