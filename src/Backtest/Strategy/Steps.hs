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


-- hmm... maybe I should also use a minimum of the percentage
-- no, because that will go way too far down
-- hmm... some maximum percent
-- or just get the start right.
withdrawalFloor :: USD (Amt Withdrawal) -> Pct Withdrawal -> Balances -> USD (Amt Withdrawal)
withdrawalFloor old raise bal = 
  let raised = amount raise (total bal)
  in max old raised


-- withdrawalPeak :: Portfolio USD Bal -> 
-- withdrawalPeak peak 


-- what was the PREVIOUS withdrawal?
-- it's not based on the current state of the portfolio
-- but based on your history

-- hmm..... 
-- really, not based on anything?
-- it's intuitive though
-- but that's not what I want to go for
-- I want 

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










