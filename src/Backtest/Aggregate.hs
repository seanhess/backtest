module Backtest.Aggregate where



import Backtest.Prelude
import Backtest.Types
import qualified Data.List as List



medianWithdrawals :: [SimResult] -> [MedianWithdrawal]
medianWithdrawals srs = 
  map toMed $ List.transpose $ map (.years) srs
  where
    toMed yrs = MedianWithdrawal
      { yearIndex = (head yrs).yearIndex
      , withdrawal = median $ map (.withdrawal) yrs
      , netExpenses = median $ map (.netExpenses) yrs
      }




withdrawalResults :: [USD (Amt Withdrawal)] -> WithdrawalResults
withdrawalResults wds =
    Histogram
        { low = minimum wds
        , med = median wds
        , init = head wds
        , p10 = percentile 0.10 wds
        , p25 = percentile 0.25 wds
        , p75 = percentile 0.75 wds
        , p90 = percentile 0.90 wds
        }
    where


withdrawalSpread :: USD (Bal Total) -> [USD (Amt Withdrawal)] -> WithdrawalSpread Int
withdrawalSpread start wds =
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

lowWithdrawals :: USD (Bal Total) -> Pct Withdrawal -> Pct Withdrawal -> [USD (Amt Withdrawal)] -> Int
lowWithdrawals start low high wds =
    let l = amount low start
        h = amount high start
    in length $ filter (\w -> l <= w && w < h) wds



aggregateResults :: [SimResult] -> [WithdrawalResults]
aggregateResults srs = srs
  & map (.years)
  & List.transpose
  & map (map (.withdrawal))
  & map withdrawalResults


aggregateResultsAll :: [WithdrawalResults] -> WithdrawalResults
aggregateResultsAll wrs = Histogram
  { low = minimum $ map (.low) wrs
  , init = minimum $ map (.init) wrs
  , med = median $ map (.med) wrs
  , p10 = median $ map (.p10) wrs
  , p25 = median $ map (.p10) wrs
  , p75 = median $ map (.p75) wrs
  , p90 = median $ map (.p90) wrs
  }

-- aggregateMedian :: [WithdrawalResults] -> WithdrawalResults
-- aggregateMedian wrs = aggregateResults median wrs

-- aggregatePercentile :: Float -> [WithdrawalResults] -> WithdrawalResults
-- aggregatePercentile p wrs = aggregateResults (percentile p) wrs


aggregateSpread :: [WithdrawalSpread Int] -> AggregateSpread
aggregateSpread ws' =
  AggregateSpread
    { totalSpread = totalSpread' ws'
    , worstSpread = worstSpread' ws'
    , numSamples = numSamples' ws'
    }
  where
    totalSpread' ws =
      WithdrawalSpread
        { wlow = sum $ map (.wlow) ws
        , w2_0 = sum $ map (.w2_0) ws
        , w2_5 = sum $ map (.w2_5) ws
        , w3_0 = sum $ map (.w3_0) ws
        , w3_5 = sum $ map (.w3_5) ws
        , w4_0 = sum $ map (.w4_0) ws
        , w4_5 = sum $ map (.w4_5) ws
        , w5_0 = sum $ map (.w5_0) ws
        , w5_5 = sum $ map (.w5_5) ws
        , whigh = sum $ map (.whigh) ws
        }

    worstSpread' :: [WithdrawalSpread Int] -> WithdrawalSpread Int
    worstSpread' ws =
      List.head $ List.sortOn spreadPoints ws


    numSamples' :: [WithdrawalSpread Int] -> WithdrawalSpread (Pct Success)
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


    pctSamples :: Int -> (WithdrawalSpread Int -> Int) -> [WithdrawalSpread Int] -> Pct Success
    pctSamples tot toSpread ws =
      pctFromFloat $ (fromIntegral (length (filter (hasSpread toSpread) ws)) / fromIntegral tot)

-- count 15s highest, then 20s etc
spreadPoints :: WithdrawalSpread Int -> Int
spreadPoints ws = negate $ ws.wlow * 10000 + ws.w2_0 * 1000 + ws.w2_5 * 100 + ws.w3_0 * 10 + ws.w3_5

hasSpread :: (WithdrawalSpread Int -> Int) -> WithdrawalSpread Int -> Bool
hasSpread toSpread wd = (toSpread wd) /= 0

yearSpread :: [SimResult] -> WithdrawalSpread [Year]
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
    years f = map (.startYear) $ filter (hasSpread f . (.wdSpread)) srs




averageEndPortfolio :: [SimResult] -> USD (Bal Total)
averageEndPortfolio srs =
    let tots = map (total . (.endBalance)) srs
    in fromCents $ round $ (fromIntegral $ sum $ map (totalCents) tots :: Float) / (fromIntegral $ length tots :: Float) :: USD (Bal Total)

-- Median END Balance, not portfolio
medianEndPortfolio :: [SimResult] -> USD (Bal Total)
medianEndPortfolio srs =
    let tots = map (total . (.endBalance)) srs
    in fromCents $ median $ map totalCents tots



median :: (Ord a) => [a] -> a   
median [] = error "Median of empty list"
median xs = sort xs !! mid   
 where mid = (length xs) `div` 2 

percentile :: (Ord a) => Float -> [a] -> a   
percentile _ [] = error "Percentile of empty list"
percentile p xs = sort xs !! n
 where n = round $ (fromIntegral $ length xs) * p





