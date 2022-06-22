{-# LANGUAGE PolyKinds #-}
module Backtest.Strategy.ABW where

-- Amortization Based Withdrawals

import Backtest.Prelude
import Backtest.Types hiding (history, nextYear)
import Backtest.Types.Usd as Usd
import Backtest.Types.Pct as Pct
import Backtest.Strategy (staticWithdrawal, thousand60, withdraw4, rebalancePct)
import Backtest.Simulation
import Backtest.History (compoundStockReturn, priorYears, priorYears, priorYears, priorYears, priorYears, priorYears, priorYears, priorYears)
import qualified Data.List.NonEmpty as NE

import Debug.Trace (traceM, trace)










withdrawABW :: Actions ()
withdrawABW = do
    h <- now
    bal <- balances
    yl <- yearsLeft
    withdraw $ amortizedWithdrawal yl h.cape bal

amortizedWithdrawal :: NumYears -> CAPE -> Balances -> USD (Amt Withdrawal)
amortizedWithdrawal yrs cape bal =
  let retStk = estimatedReturnStocks cape
      retBnd = estimatedReturnBonds
  in amortize (estimatedReturnTotal bal retBnd retStk) yrs (total bal)

amortize :: Pct (Return Total) -> NumYears -> USD (Bal Total) -> USD (Amt Withdrawal)
amortize ret yrs tot =
  let wdp = calcWithdrawal yrs ret
  in amount wdp tot


withdrawABWDips :: Actions ()
withdrawABWDips = do
    h <- now
    hs <- history
    bal <- balances
    yl <- yearsLeft
    let rets = returnsWithRecentHistory yl bal h hs
    -- traceM $ show (h.year, rets)
    let wda = pmtFluctuate rets (total bal)
    withdraw wda


returnsWithRecentHistory :: NumYears -> Balances -> History -> NonEmpty History -> [Pct (Return Total)]
returnsWithRecentHistory yl bal cur hs =
    let retStk = estimatedReturnStocks cur.cape :: Pct (Return Stocks)
        bonds  = replicate yl estimatedReturnBonds
        depth  = estimatedCrashDepth $ compoundStockReturn $ take 5 $ priorYears cur.year hs
        retStkAdj = adjustedReturn depth retStk
        stocks = depth : replicate (duration-1) (pct 0) <> replicate (yl-duration) retStkAdj
    in zipWith (estimatedReturnTotal bal) bonds stocks
    -- in map fromPct stocks
  where 
    duration = 10
    adjustedReturn d av = (av * fromIntegral yl - d) / (fromIntegral $ yl - duration)
        

estimatedCrashDepth :: Pct (Return Stocks) -> Pct (Return Stocks)
estimatedCrashDepth retPast = min (pct 0) $ max crash crashMod
    where
      crash = pct (-40.0)
      crashPast = min 0 retPast    -- only negative portion of recent return
      crashMod = crash - crashPast -- -30 - -20 = -10




-- withdrawABW' :: Actions ()
-- withdrawABW' = do
--     h <- history
--     bal <- balances
--     yl <- yearsLeft
--     let er = estimatedReturnTotal bal h.cape
--     -- let hs = badHistory wr h.cape
--     -- let rets = take (yl-1) $ historicalReturns bal hs
--     let rets = take (yl-1) $ badReturns yl er
--     let wda = pmtFluctuate rets (total bal)
--     traceM $ show ("yl", yl, "rets", rets, "wda", wda, total bal)
--     withdraw wda


-- I want to take the actual worst historical returns based on that CAPE ratio
-- and see if we can survive that
-- worst = lowest ending balance?
-- worst = lowest compound return (no, with withdrawals)
badReturns :: NumYears -> Pct (Return Total) -> [Pct (Return Total)]
badReturns yl er =
  take (yl-1) $ earlyCrash <> flat 6 <> rets
  -- take (yl-1) $ flat 8 <> rets

  where
    earlyCrash = [pct (-10), pct (-20)] :: [Pct (Return Total)]
    flat     n = replicate n (pct 0)
    rets       = cycle [er]





-- | estimates total return given the CAPE ratio and your current portfolio
-- this is too high. It's overdoing it a lot
estimatedReturnTotal :: Balances -> Pct (Return Bonds) -> Pct (Return Stocks) -> Pct (Return Total)
estimatedReturnTotal (Portfolio (USD 0) (USD 0)) _ _ = 0
estimatedReturnTotal bal rb rs =
  let ps = pctStocks bal
      pb = pctBonds ps
  in totalReturn
    [ weightedReturn ps rs
    , weightedReturn pb rb
    ]

estimatedReturnStocks :: CAPE -> Pct (Return Stocks)
estimatedReturnStocks (CAPE r) = pctFromFloat (1/r)

-- SOMEDAY get interest rate data and come up with a better formula
-- estimate 5% nominal on average? 5.33% long term average according to vanguard
-- 2% real according to bogleheads
estimatedReturnBonds :: Pct (Return Bonds)
estimatedReturnBonds = pct 2




-- | current withdrawal% amortized for remaining lifespan
calcWithdrawal :: NumYears -> Pct (Return Total) -> Pct Withdrawal
calcWithdrawal n (Pct 0) = Pct $ (1 / fromIntegral n)
calcWithdrawal n rp =
  pctFromFloat $ pmt' (Pct.toFloat rp) n 1


calcNPV :: Pct (Return Total) -> [USD (Amt a)] -> USD (Bal b)
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
  -> USD (Bal Total)   -- ^ Net Present Value (loan amount)
  -> USD (Amt Withdrawal)   -- ^ Payment
pmtFluctuate rets bal =
  -- guess, calculate remainder, subtract from guess
  -- go until you reach the desired difference
  let n = length rets + 1
      er = average rets
      w = amortize er n bal :: USD (Amt Withdrawal)
  in pmtFluctuate' rets bal w


pmtFluctuate' :: [Pct (Return Total)] -> USD (Bal Total) -> USD (Amt Withdrawal) -> USD (Amt Withdrawal)
pmtFluctuate' rets bal wstart = 
  let periods = length rets + 1
      ws = take 15 $ drop 1 $ iterate (findWithdrawal periods) $ FlucWD (usd 0) 0 (usd 0) wstart wstart 0
  in fromMaybe wstart $ lastMay $ fmap (.withdrawal) ws

  where


    nextWithdrawal :: FlucWD -> Int -> USD (Amt Withdrawal) -> USD (Amt Withdrawal)
    nextWithdrawal f left w
      | left <  0  = avg f.low w
      | left >  0  = avg f.high w
      | otherwise  = w

    lowWithdrawal :: FlucWD -> Int -> USD (Amt Withdrawal) -> USD (Amt Withdrawal)
    lowWithdrawal f left w
      | left >  0  = w
      | otherwise  = f.low

    highWithdrawal :: FlucWD -> Int -> USD (Amt Withdrawal) -> USD (Amt Withdrawal)
    highWithdrawal f left w
      | left <  0  = w
      | otherwise  = f.high

    findWithdrawal :: NumYears -> FlucWD -> FlucWD
    findWithdrawal periods f =
      let w = f.next
          count = f.count + 1
          withdrawal = w
          left = runReturns rets w (totalCents bal) :: Int

          low  = lowWithdrawal f left w
          high = highWithdrawal f left w

          next = nextWithdrawal f left w

          fwd = FlucWD {..}
      -- in trace (show fwd) $ fwd
      in fwd


data FlucWD = FlucWD
  { withdrawal :: USD (Amt Withdrawal)
  , left :: Int
  , low  :: USD (Amt Withdrawal)
  , high :: USD (Amt Withdrawal)
  , next :: USD (Amt Withdrawal)
  , count :: Int
  } deriving (Show)


-- wait, but what about the first year?
runReturns :: [Pct (Return Total)] -> USD (Amt Withdrawal) -> Int -> Int
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





