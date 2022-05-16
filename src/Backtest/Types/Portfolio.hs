module Backtest.Types.Portfolio where

import Backtest.Prelude
import Backtest.Types.Usd
import Backtest.Types.Pct as Pct


data Portfolio f = Portfolio
  { stocks :: USD (f Stocks)
  , bonds  :: USD (f Bonds)
  } deriving (Show, Eq)

type Balances = Portfolio Bal
type Changes  = Portfolio Amt

instance Semigroup (Portfolio f) where
  Portfolio s b <> Portfolio s' b' = Portfolio (s <> s') (b <> b')

total :: Portfolio f -> USD (f Total)
total b = USD $
    (totalCents b.stocks)
  + (totalCents b.bonds)

changes :: Balances -> Balances -> Changes
changes start end =
    Portfolio (gains start.stocks end.stocks) (gains start.bonds end.bonds)


allocationStocks :: Balances -> Pct Stocks
allocationStocks bal = fromPct $ percentOf bal.stocks (total bal)

pctBonds :: Pct Stocks -> Pct Bonds
pctBonds (Pct s) = Pct (1 - s)






