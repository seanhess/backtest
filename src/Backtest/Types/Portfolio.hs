module Backtest.Types.Portfolio where

import Backtest.Prelude
import Backtest.Types.Usd
import Backtest.Types.Pct as Pct


data Portfolio f = Portfolio
  { stocks :: USD f Stocks
  , bonds  :: USD f Bonds
  } deriving (Show, Eq)

type Balances = Portfolio Bal
type Changes  = Portfolio Amt

instance Semigroup (Portfolio f) where
  Portfolio s b <> Portfolio s' b' = Portfolio (s <> s') (b <> b')

total :: Portfolio f -> USD f Total
total b = USD $
    (totalCents b.stocks)
  + (totalCents b.bonds)

changes :: Balances -> Balances -> Changes
changes start end =
    Portfolio (gains start.stocks end.stocks) (gains start.bonds end.bonds)



-- higher level functions with balances, amounts, etc

gains :: USD f a -> USD f a -> USD Amt a
gains (USD s) (USD e) = USD $ abs e - abs s

diff :: USD f a -> USD f a -> USD Amt a
diff (USD a) (USD b) = USD $ abs (a - b)

gainsPercent :: USD f a -> USD f a -> Pct a
gainsPercent s e =
    Pct $ (fromIntegral e.totalCents) / (fromIntegral s.totalCents) - 1

-- | balances can never be zero, but returns can
balance :: Int -> USD Bal a
balance n = minZero (USD n)

-- | Applies a return to a balance
addToBalance :: USD Amt a -> USD Bal b -> USD Bal b
addToBalance (USD ret) (USD b) = balance $ b + ret

addAmounts :: USD Amt a -> USD Amt a -> USD Amt a
addAmounts (USD a) (USD b) = USD $ a + b

percentOf :: USD f a -> USD Bal b -> Pct a
percentOf (USD a) (USD bal) = pctFromFloat (fromIntegral a / fromIntegral bal)

amount :: Pct ass -> USD Bal b -> USD Amt ass
amount p bal = fromFloat $
    (fromIntegral $ totalCents bal) * Pct.toFloat p / 100

amountBalance :: Pct ass -> USD Bal b -> USD Bal ass
amountBalance p bal = fromUSD $ amount p bal

amountWithdrawal :: Pct Withdrawal -> USD Bal b -> USD Amt Withdrawal
amountWithdrawal p bal = fromUSD $ amount p bal
