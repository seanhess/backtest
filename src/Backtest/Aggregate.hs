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
        , w20p = sum $ map (.w20p) ws
        , w25p = sum $ map (.w25p) ws
        , w30p = sum $ map (.w30p) ws
        , w35p = sum $ map (.w35p) ws
        }

    worstSpread' :: [WithdrawalSpread] -> WithdrawalSpread
    worstSpread' ws =
      List.head $ List.sortOn spreadPoints ws

    -- count 15s highest, then 20s etc
    spreadPoints ws = negate $ ws.wlow * 10000 + ws.w20p * 1000 + ws.w25p * 100 + ws.w30p * 10 + ws.w35p

    numSamples' :: [WithdrawalSpread] -> WithdrawalSpread
    numSamples' ws =
      WithdrawalSpread
        { wlow = length $ filter (hasSpread (.wlow)) ws
        , w20p = length $ filter (hasSpread (.w20p)) ws
        , w25p = length $ filter (hasSpread (.w25p)) ws 
        , w30p = length $ filter (hasSpread (.w30p)) ws 
        , w35p = length $ filter (hasSpread (.w35p)) ws 
        }

    hasSpread :: (WithdrawalSpread -> Int) -> WithdrawalSpread -> Bool
    hasSpread toSpread wd = (toSpread wd) /= 0

