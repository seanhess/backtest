{-# LANGUAGE PolyKinds #-}
module Backtest.Strategy.ABW where

-- Amortization Based Withdrawals

import Backtest.Prelude
import Backtest.Types.Usd as Usd
import Backtest.Types.Pct as Pct
import Backtest.Types.History (CAPE(..))
import Backtest.Types.Portfolio (Balances, amount, total, gains, addToBalance, addAmounts, diff, Portfolio(..))
import Backtest.Strategy (pctBonds, allocationStocks, staticWithdrawal)
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
  amortize (estimatedReturnTotal bal cape) yrs (total bal)


amortize :: Pct (Return Total) -> YearsLeft -> USD Bal Total -> USD Amt Withdrawal
amortize ret yrs tot =
  let wdp = calcWithdrawal yrs ret
  in amount wdp tot


-- the real problem is:
-- 1/CAPE is too aggressive if your portfolio is drawn down
-- what does drawn down mean?
-- if you are way below 4% of current portfolio

withdrawABW' :: Actions ()
withdrawABW' = do
    h <- history
    bal <- balances
    yl <- yearsLeft
    let er = estimatedReturnTotal bal h.cape
    let rets = replicate (yl-1) er
    let wda = pmtFluctuate rets (total bal)
    traceM $ show ("yl", yl, "er", er, "rets", rets, "wda", wda, total bal)
    withdraw wda




-- | estimates total return given the CAPE ratio and your current portfolio
-- this is too high. It's overdoing it a lot
estimatedReturnTotal :: Balances -> CAPE -> Pct (Return Total)
estimatedReturnTotal (Portfolio (USD 0) (USD 0)) _ = 0
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

-- bumpReturn :: Pct (Return Total) -> Pct (Return Total)
-- bumpReturn p = p - (pct 0.5)


-- | current withdrawal% amortized for remaining lifespan
calcWithdrawal :: YearsLeft -> Pct (Return Total) -> Pct Withdrawal
calcWithdrawal n (Pct 0) = Pct $ (1 / fromIntegral n)
calcWithdrawal n rp =
  pctFromFloat $ pmt' (Pct.toFloat rp) n 1


calcNPV :: Pct (Return Total) -> [USD Amt a] -> USD Bal b
calcNPV ret amts =
  let as = map (fromIntegral . totalCents) amts :: [Float]
  in fromCents $ round $ netPresentValue (Pct.toFloat ret) as


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


           





-- start with the average return and ABW?
pmtFluctuate
  :: [Pct (Return Total)] -- ^ Returns over time
  -> USD Bal Total   -- ^ Net Present Value (loan amount)
  -> USD Amt Withdrawal   -- ^ Payment
pmtFluctuate rets bal =
  -- guess, calculate remainder, subtract from guess
  -- go until you reach the desired difference
  let n = length rets + 1
      er = average rets
      w = amortize er n bal :: USD Amt Withdrawal
  in pmtFluctuate' rets bal w


pmtFluctuate' :: [Pct (Return Total)] -> USD Bal Total -> USD Amt Withdrawal -> USD Amt Withdrawal
pmtFluctuate' rets bal wstart = 
  let periods = length rets + 1
      ws = take 15 $ drop 1 $ iterate (findWithdrawal periods) $ FlucWD Nothing 0 (usd 0) wstart wstart 0
  in last $ mapMaybe (.withdrawal) ws

  where


    nextWithdrawal :: FlucWD -> Int -> USD Amt Withdrawal -> USD Amt Withdrawal
    nextWithdrawal f left w
      | left <  0  = avg f.low w
      | left >  0  = avg f.high w
      | otherwise  = w

    lowWithdrawal :: FlucWD -> Int -> USD Amt Withdrawal -> USD Amt Withdrawal
    lowWithdrawal f left w
      | left >  0  = w
      | otherwise  = f.low

    highWithdrawal :: FlucWD -> Int -> USD Amt Withdrawal -> USD Amt Withdrawal
    highWithdrawal f left w
      | left <  0  = w
      | otherwise  = f.high

    avg :: USD f a -> USD f a -> USD f a
    avg a b = fromCents $ (totalCents a + totalCents b) `div` 2

    findWithdrawal :: YearsLeft -> FlucWD -> FlucWD
    findWithdrawal periods f =
      let w = f.next
          count = f.count + 1
          withdrawal = Just w
          left = runReturns rets w (totalCents bal) :: Int

          low  = lowWithdrawal f left w
          high = highWithdrawal f left w

          next = nextWithdrawal f left w

          fwd = FlucWD {..}
      -- in trace (show ("w", w, "l", left, "e", extra, "n", next)) $ (w, extra, next)
      -- in trace (show fwd) $ fwd
      in fwd


data FlucWD = FlucWD
  { withdrawal :: Maybe (USD Amt Withdrawal)
  , left :: Int
  , low  :: USD Amt Withdrawal
  , high :: USD Amt Withdrawal
  , next :: USD Amt Withdrawal
  , count :: Int
  } deriving (Show)


-- wait, but what about the first year?
runReturns :: [Pct (Return Total)] -> USD Amt Withdrawal -> Int -> Int
runReturns rs w b =
  (foldl nextYear b rs) - (totalCents $ gain w)

  where
    nextYear :: Int -> Pct (Return a) -> Int
    nextYear bc r = 
      let aw = bc - (totalCents $ gain w) :: Int
          ye = round $ (1 + (Pct.toFloat r)) * fromIntegral aw
      in if aw > 0
          then ye
          else aw





