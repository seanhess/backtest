module Backtest.Strategy where

import Backtest.Prelude
import Backtest.Types



pctBonds :: Pct Stocks -> Pct Bonds
pctBonds (Pct s) = Pct (1 - s)

staticWithdrawal :: Pct Withdrawal -> Balances -> USD Amt Withdrawal 
staticWithdrawal p bal1 =
    amount p (total bal1)

rebalanceFixed :: Pct Stocks -> Balances -> Balances
rebalanceFixed ps bal =
    let tot = total bal
        pb = pctBonds ps
        ts = amountBalance ps tot :: USD Bal Stocks
        tb = amountBalance pb tot :: USD Bal Bonds
    in Portfolio ts tb







-- This is both the withdrawal AND the rebalancing
rebalancePrime :: USD Bal Stocks -> Balances -> Balances
rebalancePrime start bal =
    let target = amountBalance (pct 120) start
        extra = minZero $ gains target bal.stocks :: USD Amt Stocks
        ds = loss extra
        db = gain $ toBonds extra
    in Portfolio (addToBalance ds bal.stocks) (addToBalance db bal.bonds)

rebalancePrimeNew :: USD Bal Stocks -> Balances -> Balances
rebalancePrimeNew start bal
  | bal.stocks > amountBalance (pct 120) start = rebalancePrime start bal
  | bal.stocks < amountBalance (pct 80) start = primeBuyStocks start (pct 80) bal
  | otherwise = bal

primeBuyStocks :: USD Bal Stocks -> Pct Stocks -> Balances -> Balances
primeBuyStocks start p bal =
    let target = amountBalance p start
        short  = minZero $ gains bal.stocks target
        actual = min short (toStocks $ toAmount bal.bonds)
    in Portfolio (addToBalance (gain actual) bal.stocks) (addToBalance (loss $ toBonds actual) bal.bonds)






-- Larry Swedroe 5/25 bands
rebalance525Bands :: Pct Stocks -> Balances -> Balances
rebalance525Bands ps bal
    | diffAbsPercent ps bal >= 5 = rebalanceFixed ps bal
    | diffRelPercent ps bal >= 25 = rebalanceFixed ps bal
    | otherwise = bal

diffAbsPercent :: Pct Stocks -> Balances -> Pct Stocks
diffAbsPercent ps bal =
    abs (allocationStocks bal - ps)

diffRelPercent :: Pct Stocks -> Balances -> Pct Stocks
diffRelPercent ps bal =
    abs $ (allocationStocks bal) / ps - 1

allocationStocks :: Balances -> Pct Stocks
allocationStocks bal = percentOf bal.stocks (total bal)