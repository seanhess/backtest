module Backtest.Aggregate where



import Backtest.Prelude
import Backtest.Types
import qualified Data.List as List


aggregateWithdrawals :: [WithdrawalSpread Int] -> AggregateWithdrawals
aggregateWithdrawals ws' =
  AggregateWithdrawals
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
        , whigh = sum $ map (.whigh) ws
        }

    worstSpread' :: [WithdrawalSpread Int] -> WithdrawalSpread Int
    worstSpread' ws =
      List.head $ List.sortOn spreadPoints ws

    -- count 15s highest, then 20s etc
    spreadPoints ws = negate $ ws.wlow * 10000 + ws.w2_0 * 1000 + ws.w2_5 * 100 + ws.w3_0 * 10 + ws.w3_5

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
        , whigh = pctSamples tot (.whigh) ws
        }


    pctSamples :: Int -> (WithdrawalSpread Int -> Int) -> [WithdrawalSpread Int] -> Pct Success
    pctSamples tot toSpread ws =
      pctFromFloat $ (fromIntegral (length (filter (hasSpread toSpread) ws)) / fromIntegral tot)

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
    , whigh = years (.whigh)
    }
  where
    years f = map (.startYear) $ filter (hasSpread f . (.wdSpread)) srs