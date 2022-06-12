module Backtest.Strategy.Peak where


import Backtest.Prelude
import Backtest.Types
import Data.List (maximumBy)
import Backtest.Strategy (staticWithdrawal)
import Backtest.Simulation (Actions, withdraw, balances)
import qualified Data.List.NonEmpty as NE

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


-- wait. this should just calculate the portfolio FROM the recent peak
-- withdrawPeak :: (Balances -> Balances) -> Pct Withdrawal -> Actions ()
-- withdrawPeak findPeakHistory pw = do
--   bals <- pastStartBalances
--   bal <- balances
--   let peak = fromMaybe (findPeakHistory bal) $ pastPeak bals
--   let wda = staticWithdrawal pw peak
--   withdraw wda


-- this is the withdrawal percentage compared to CURRENT portfolio
-- peakWithdrawal :: (Balances -> Balances) -> Pct Withdrawal -> Balances -> USD (Amt Withdrawal)
-- peakWithdrawal findPeak wp bal =
--   staticWithdrawal wp (findPeak bal)


-- your highest balance ever
-- pastPeak :: [Balances] -> Maybe Balances
-- pastPeak bals = headMay $ reverse $ sortOn total bals

peakWithdrawal :: NonEmpty History -> Pct Withdrawal -> Balances -> USD (Amt Withdrawal)
peakWithdrawal time swr start = amount swr $ total $ peakBalance time start

-- | all years starting now into the past
reverseTimeline :: Year -> NonEmpty History -> NonEmpty History
reverseTimeline y hs = reverse $ fromMaybe [head hs] $ nonEmpty $ NE.filter isPast hs
  where
    isPast :: History -> Bool
    isPast h = h.year <= y

-- | what is the maximum value of my portfolio in the past?
peakBalance :: NonEmpty History -> Balances -> Balances
peakBalance past bal =
  maximumBy (comparing total) $ fmap (pastPortfolio . pastGains (head past)) past
  where
    pastPortfolio :: Portfolio Pct Gains -> Balances
    pastPortfolio gs = applyGains bal gs

    pastGains :: History -> History -> Portfolio Pct Gains
    pastGains now old =
      gainsPortfolio now.values old.values


