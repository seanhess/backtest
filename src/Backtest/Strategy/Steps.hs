module Backtest.Strategy.Steps where


import Backtest.Prelude
import Backtest.Simulation
import Backtest.Types
import Backtest.Strategy
import Backtest.Strategy.ABW
import Debug.Trace



withdrawFloor :: USD (Amt Withdrawal) -> Pct Withdrawal -> Actions ()
withdrawFloor start raise = do
  old <- fromMaybe start <$> lastWithdrawal
  bal <- balances
  withdraw $ withdrawalFloor old raise bal


withdrawalFloor :: USD (Amt Withdrawal) -> Pct Withdrawal -> Balances -> USD (Amt Withdrawal)
withdrawalFloor old raise bal = 
  let raised = amount raise (total bal)
  in max old raised


withdrawRaised :: USD (Amt Withdrawal) -> Pct Withdrawal -> Pct Raise -> Actions ()
withdrawRaised start swr raise = do
  old <- fromMaybe start <$> lastWithdrawal
  bal <- balances
  withdraw $ raisedWithdrawal old swr raise bal

-- how do we know when we can raise?
data Raise
raisedWithdrawal :: USD (Amt Withdrawal) -> Pct Withdrawal -> Pct Raise -> Balances -> USD (Amt Withdrawal)
raisedWithdrawal old swr raise bal =
  let maxw = amount swr (total bal) :: USD (Amt Withdrawal)
      raised = toWithdrawal $ amount raise (toBalance old) :: USD (Amt Withdrawal)
  in if maxw > old
    then min maxw raised
    else old




-- TODO I need the last withdrawal, or the history of withdrawals
withdrawSteps :: USD (Amt Withdrawal) -> Actions ()
withdrawSteps start = do
  -- What was my last withdrawal?
  ow <- lastWithdrawal
  yl <- yearsLeft
  bal <- balances
  h <- now
  let abw = amortizedWithdrawal yl h.cape bal
      old = fromMaybe start ow
      low = amount (pct 90) $ fromUSD old
      high = amount (pct 102) $ fromUSD old
  -- traceM $ show (h.year, "old:", ow, low, high, steppedWithdrawal abw old low high)
  withdraw $ steppedWithdrawal abw old low high
  where
    steppedWithdrawal abw old low high
      | abw < low = low
      | abw > high = high
      | otherwise = old










