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

instance Monoid (Portfolio USD f) where
  mempty = Portfolio mempty mempty

instance Semigroup (Portfolio Pct f) where
  Portfolio s b <> Portfolio s' b' = Portfolio (s <> s') (b <> b')

instance Monoid (Portfolio Pct f) where
  mempty = Portfolio mempty mempty

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

addChanges :: Changes -> Balances -> Balances
addChanges ch bal =
    Portfolio (addToBalance ch.stocks bal.stocks) (addToBalance ch.bonds bal.bonds)

pctStocks :: Balances -> Pct Stocks
pctStocks bal = fromPct $ percentOf bal.stocks (total bal)

pctBonds :: Pct Stocks -> Pct Bonds
pctBonds (Pct s) = Pct (1 - s)


data Gains a

-- we want to maximum the GAINS, no?
gainsPortfolio :: Balances -> Balances -> Portfolio Pct Gains
gainsPortfolio start end = Portfolio
  { stocks = gainsPercent start.stocks end.stocks
  , bonds = gainsPercent start.bonds end.bonds
  }

applyGains :: Balances -> Portfolio Pct Gains -> Balances
applyGains bal gs = Portfolio
  { stocks = addToBalance (amount gs.stocks bal.stocks) bal.stocks
  , bonds = addToBalance (amount gs.bonds bal.bonds) bal.bonds
  }



-- all available allocations. These aren't completely discrete amounts, it doesn't need to be like that
data Allocation
  = S100
  | S95
  | S90
  | S85
  | S80
  | S75
  | S70
  | S65
  | S60
  | S55
  | S50
  | S45
  | S40
  | S35
  | S30
  | S25
  | S20
  | S15
  | S10
  | S05
  | S00
  deriving (Show, Eq, Enum, Bounded)

allocToPct :: Allocation -> Pct Stocks
allocToPct S100 = pct 100
allocToPct S95  = pct 95
allocToPct S90  = pct 90
allocToPct S85  = pct 85
allocToPct S80  = pct 80
allocToPct S75  = pct 75
allocToPct S70  = pct 70
allocToPct S65  = pct 65
allocToPct S60  = pct 60
allocToPct S55  = pct 55
allocToPct S50  = pct 50
allocToPct S45  = pct 45
allocToPct S40  = pct 40
allocToPct S35  = pct 35
allocToPct S30  = pct 30
allocToPct S25  = pct 25
allocToPct S20  = pct 20
allocToPct S15  = pct 15
allocToPct S10  = pct 10
allocToPct S05  = pct 05
allocToPct S00  = pct 00




