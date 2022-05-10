{-# LANGUAGE PolyKinds #-}
module Backtest.Strategy.ABW where

-- Amortization Based Withdrawals

import Backtest.Prelude
import Backtest.Types.Usd as Usd
import Backtest.Types.Pct as Pct
import Backtest.Types.History (CAPE(..))
import Backtest.Types.Portfolio (Balances, amount, total)
import Backtest.Strategy (pctBonds, allocationStocks)
import Backtest.Simulation (bondsFirst, Actions, history, balances, yearsLeft, withdraw)

import Debug.Trace (traceM, trace)






data Return a
data Weighted
type YearsLeft = Int

withdrawABW :: Actions ()
withdrawABW = do
    h <- history
    bal <- balances
    yl <- yearsLeft
    withdraw $ amortizedWithdrawal yl h.cape bal

amortizedWithdrawal :: YearsLeft -> CAPE -> Balances -> USD Amt Withdrawal
amortizedWithdrawal yrs cape bal =
  let wdp = calcWithdrawal yrs $ estimatedReturnTotal bal cape :: Pct Withdrawal
  in amount wdp (total bal)



-- TODO calculate allocation (already done?)
-- Strategy.allocationStocks

-- | estimates total return given the CAPE ratio and your current portfolio
estimatedReturnTotal :: Balances -> CAPE -> Pct (Return Total)
estimatedReturnTotal bal cape =
  let ps = allocationStocks bal
      pb = pctBonds ps
  in totalReturn
    [ weightedReturn ps $ estimatedReturnStocks cape
    , weightedReturn pb estimatedReturnBonds
    ]

estimatedReturnStocks :: CAPE -> Pct (Return Stocks)
estimatedReturnStocks (CAPE r) = pctFromFloat (1/r)

-- 5% real worldwide return via bogleheads
estimatedReturnStocks NA       = pct 5

-- SOMEDAY get interest rate data and come up with a better formula
-- estimate 5% nominal on average? 5.33% long term average according to vanguard
-- 2% real according to bogleheads
estimatedReturnBonds :: Pct (Return Bonds)
estimatedReturnBonds = pct 2

weightedReturn :: Pct a -> Pct (Return a) -> Pct (Return Weighted)
weightedReturn alloc ret =
  fromPct $ alloc * fromPct ret

totalReturn :: [Pct (Return Weighted)] -> Pct (Return Total)
totalReturn rs = fromPct $ sum rs



-- | current withdrawal% amortized for remaining lifespan
calcWithdrawal :: YearsLeft -> Pct (Return Total) -> Pct Withdrawal
calcWithdrawal n rp =
  pctFromFloat $ pmt' (Pct.toFloat rp) n 1



-- https://www.bogleheads.org/wiki/Amortization_based_withdrawal_formulas
-- P = (Pv*R) / [1 - (1 + R)^(-n)]
-- Excel: PMT(R, N, -Pv, 0, 0)
pmt
  :: Float -- ^ Periodic Interest Rate = APR / number of interest periods per year
  -> Int   -- ^ Total interest periods (interest periods per year * number of years) 
  -> Float -- ^ The net present value (loan amount)
  -> Float -- ^ The payment
pmt r n pv =
  (pv * r) / (1 - (1 / (1+r)^n))

-- Same, but first payment starts immediately, not next period
-- Excel: PMT(R, N, -Pv, 0, -1)
pmt'
  :: Float -- ^ Periodic Interest Rate = APR / number of interest periods per year
  -> Int   -- ^ Total interest periods (interest periods per year * number of years) 
  -> Float -- ^ The net present value (loan amount)
  -> Float -- ^ The payment
pmt' r n pv =
  (pmt r n pv) / (1 + r)





-- From: https://hackage.haskell.org/package/raft-0.3.7.0/docs/Data-Function-Finance.html
netPresentValue :: (Fractional a, Num a) =>
     a   -- ^ The rate per period.
  -> [a] -- ^ The values at the end of the periods.
  -> a   -- ^ The net present value.
netPresentValue _ [] = 0
netPresentValue rate (x : xs) = (x + netPresentValue rate xs) / (1 + rate)