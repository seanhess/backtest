module Backtest.Strategy where

import Backtest.Prelude
import Backtest.Types
import Backtest.Simulation (Actions, withdraw, rebalance)
import Backtest.Types.Portfolio (changes)



staticWithdrawal :: Pct Withdrawal -> Balances -> USD (Amt Withdrawal) 
staticWithdrawal p bal1 =
    amount p (total bal1)

rebalanceFixed :: Allocation -> Balances -> Balances
rebalanceFixed al bal =
    let tot = total bal
        ps = fromAlloc al
        pb = pctBonds ps
        ts = toBalance $ amount ps tot :: USD (Bal Stocks)
        tb = toBalance $ amount pb tot :: USD (Bal Bonds)
    in Portfolio ts tb







-- This is both the withdrawal AND the rebalancing
rebalancePrime :: USD (Bal Stocks) -> Balances -> Balances
rebalancePrime start bal =
    let target = toBalance $ amount (pct 120) start
        extra = minZero $ gains target bal.stocks :: USD (Amt Stocks)
        ds = loss extra
        db = gain $ toBonds extra
    in Portfolio (addToBalance ds bal.stocks) (addToBalance db bal.bonds)

rebalancePrimeNew :: USD (Bal Stocks) -> Balances -> Balances
rebalancePrimeNew start bal
  | bal.stocks > (toBalance $ amount (pct 120) start) = rebalancePrime start bal
  | bal.stocks < (toBalance $ amount (pct 80) start) = primeBuyStocks start (pct 80) bal
  | otherwise = bal

rebalancePrimeLow :: USD (Bal Stocks) -> Balances -> Balances
rebalancePrimeLow start bal
  | bal.stocks < (toBalance $ amount (pct 80) start) = primeBuyStocks start (pct 80) bal
  | otherwise = bal

primeBuyStocks :: USD (Bal Stocks) -> Pct Stocks -> Balances -> Balances
primeBuyStocks start p bal =
    let target = toBalance $ amount p start
        short  = minZero $ gains bal.stocks target
        actual = min short (toStocks $ toAmount bal.bonds)
    in Portfolio (addToBalance (gain actual) bal.stocks) (addToBalance (loss $ toBonds actual) bal.bonds)






-- Larry Swedroe 5/25 bands
rebalance525Bands :: Allocation -> Balances -> Balances
rebalance525Bands al bal
    | diffAbsPercent al bal >= pct 5 = rebalanceFixed al bal
    | diffRelPercent al bal >= pct 25 = rebalanceFixed al bal
    | otherwise = mempty

diffAbsPercent :: Allocation -> Balances -> Pct Stocks
diffAbsPercent al bal =
    abs (pctStocks bal - (fromAlloc al))

diffRelPercent :: Allocation -> Balances -> Pct Stocks
diffRelPercent al bal =
    abs $ (pctStocks bal) / (fromAlloc al) - 1


withdraw4 :: Balances -> Actions ()
withdraw4 start = do
    let const4Percent = loss $ staticWithdrawal swr4 start :: USD (Amt Withdrawal)
    withdraw const4Percent


million :: Allocation -> Balances
million al = rebalanceFixed al $ Portfolio
  { stocks = usd $ 500*1000
  , bonds = usd $ 500*1000
  }

thousand :: Allocation -> Balances
thousand al = rebalanceFixed al $ Portfolio
  { stocks = usd $ 500
  , bonds = usd $ 500
  }

thousand50 :: Balances
thousand50 = Portfolio (usd 500) (usd 500)

thousand60 :: Balances
thousand60 = Portfolio (usd 600) (usd 400)

thousand70 :: Balances
thousand70 = Portfolio (usd 700) (usd 300)

thousand80 :: Balances
thousand80 = Portfolio (usd 800) (usd 200)

thousand90 :: Balances
thousand90 = Portfolio (usd 900) (usd 100)

swr4 :: Pct Withdrawal
swr4 = pct 4

-- wait... shoot! 
-- rebalancing has to be based on the END of the calculation
rebalancePct :: Allocation -> Actions ()
rebalancePct ps = rebalance $ rebalanceFixed ps