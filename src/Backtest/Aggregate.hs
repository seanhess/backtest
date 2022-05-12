module Backtest.Aggregate where



import Backtest.Prelude
import Backtest.Types
import qualified Data.List as List


aggregateWithdrawals :: [WithdrawalSpread] -> AggregateWithdrawals
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

    worstSpread' :: [WithdrawalSpread] -> WithdrawalSpread
    worstSpread' ws =
      List.head $ List.sortOn spreadPoints ws

    -- count 15s highest, then 20s etc
    spreadPoints ws = negate $ ws.wlow * 10000 + ws.w2_0 * 1000 + ws.w2_5 * 100 + ws.w3_0 * 10 + ws.w3_5

    numSamples' :: [WithdrawalSpread] -> WithdrawalSpread
    numSamples' ws =
      WithdrawalSpread
        { wlow = length $ filter (hasSpread (.wlow)) ws
        , w2_0 = length $ filter (hasSpread (.w2_0)) ws
        , w2_5 = length $ filter (hasSpread (.w2_5)) ws 
        , w3_0 = length $ filter (hasSpread (.w3_0)) ws 
        , w3_5 = length $ filter (hasSpread (.w3_5)) ws 
        , w4_0 = length $ filter (hasSpread (.w4_0)) ws 
        , w4_5 = length $ filter (hasSpread (.w4_5)) ws 
        , whigh = length $ filter (hasSpread (.whigh)) ws 
        }

    hasSpread :: (WithdrawalSpread -> Int) -> WithdrawalSpread -> Bool
    hasSpread toSpread wd = (toSpread wd) /= 0

