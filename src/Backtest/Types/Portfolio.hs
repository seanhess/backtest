module Backtest.Types.Portfolio where

import Backtest.Prelude
import Backtest.Types.Usd
import Backtest.Types.Pct as Pct


data Portfolio a f = Portfolio
  { stocks :: a (f Stocks)
  , bonds  :: a (f Bonds)
  }

type Balances = Portfolio USD Bal
type Changes  = Portfolio USD Amt

instance Semigroup (Portfolio USD f) where
  Portfolio s b <> Portfolio s' b' = Portfolio (s <> s') (b <> b')

instance Show (Portfolio USD f) where
  show (Portfolio s b) = show (s, b)

instance Show (Portfolio Pct f) where
  show (Portfolio s b) = show (s, b)

instance Eq (Portfolio USD f) where
  (Portfolio s b) == (Portfolio s' b') = s == s' && b == b'


total :: Portfolio USD f -> USD (f Total)
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






