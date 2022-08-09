{-# LANGUAGE DeriveAnyClass #-}
module Backtest.Types.Portfolio where

import Backtest.Prelude
import Backtest.Types.Usd
import Backtest.Types.Pct as Pct
import Data.List.NonEmpty as NE
import Juniper (ToParam)


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
  -- | S45
  -- | S40
  -- | S35
  -- | S30
  -- | S25
  -- | S20
  -- | S15
  -- | S10
  -- | S05
  -- | S00
  deriving (Show, Eq, Enum, Bounded, Read, Ord, ToParam)

fromAlloc :: Allocation -> Pct Stocks
fromAlloc S100 = pct 100
fromAlloc S95  = pct 95
fromAlloc S90  = pct 90
fromAlloc S85  = pct 85
fromAlloc S80  = pct 80
fromAlloc S75  = pct 75
fromAlloc S70  = pct 70
fromAlloc S65  = pct 65
fromAlloc S60  = pct 60
fromAlloc S55  = pct 55
fromAlloc S50  = pct 50
-- fromAlloc S45  = pct 45
-- fromAlloc S40  = pct 40
-- fromAlloc S35  = pct 35
-- fromAlloc S30  = pct 30
-- fromAlloc S25  = pct 25
-- fromAlloc S20  = pct 20
-- fromAlloc S15  = pct 15
-- fromAlloc S10  = pct 10
-- fromAlloc S05  = pct 05
-- fromAlloc S00  = pct 00

toAlloc :: Pct Stocks -> Allocation
toAlloc ps =
  fromMaybe S100 $ minimumByMay (comparing distance) [minBound..maxBound]
  where distance al = abs $ fromAlloc al - ps




