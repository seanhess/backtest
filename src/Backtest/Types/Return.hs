module Backtest.Types.Return where

import Backtest.Prelude
import Backtest.Types.Pct
import Backtest.Types.Usd (Asset(Total))

data Return a
data Weighted

weightedReturn :: Pct a -> Pct (Return a) -> Pct (Return Weighted)
weightedReturn alloc ret =
  fromPct $ alloc * fromPct ret

totalReturn :: [Pct (Return Weighted)] -> Pct (Return Total)
totalReturn rs = fromPct $ sum rs