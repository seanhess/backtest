module Backtest.Strategy.Peak where


import Backtest.Prelude
import Backtest.Types
import Data.List (maximumBy)
import Backtest.Strategy (staticWithdrawal)
import Backtest.Simulation (Actions, withdraw, pastStartBalances, balances)

-- This isn't a withdrawal strategy. This is a searching strategy
-- it uses a flat withdrawal
-- unless....
-- does it redo the calculation within the thing?
-- that seems crazy

-- EACH YEAR: we have to look back to the last peak and withdraw THAT percent


-- This doesn't work, because it's based on the value of OUR peak portfolio in the past
-- which goes down because we are withdrawing

-- how can we track OUR peak portfolio?

-- How does this actually work?
-- 1. find the projected peak portfolio in the past for year 1
-- 2. use initial pct of that
-- 3. if our portfolio is at its historical peak, then

-- we need to start with a withdrawal amount, given by historyPeak

withdrawPeak :: (Balances -> Balances) -> Pct Withdrawal -> Actions ()
withdrawPeak findPeakHistory pw = do
  bals <- pastStartBalances
  bal <- balances
  let peak = fromMaybe (findPeakHistory bal) $ pastPeak bals
  let wda = staticWithdrawal pw peak
  withdraw wda


-- this is the withdrawal percentage compared to CURRENT portfolio
peakWithdrawal :: (Balances -> Balances) -> Pct Withdrawal -> Balances -> USD (Amt Withdrawal)
peakWithdrawal findPeak wp bal =
  staticWithdrawal wp (findPeak bal)



pastPeak :: [Balances] -> Maybe Balances
pastPeak bals = headMay $ reverse $ sortOn total bals


-- what is the maximum value of my portfolio in the past?
historyPeak :: Year -> [History] -> Balances -> Balances
historyPeak cur hrs bal = fromMaybe mempty $ do
  let past = reverse $ filter isPast hrs :: [History]
  now <- headMay past

  pure $ head $ reverse $ sortOn total $ map (pastPortfolio . pastGains now) past
  where
    isPast :: History -> Bool
    isPast h = h.year <= cur

    pastPortfolio :: Portfolio Pct Gains -> Balances
    pastPortfolio gs = applyGains bal gs

    pastGains :: History -> History -> Portfolio Pct Gains
    pastGains now past =
      gainsPortfolio now.values past.values


