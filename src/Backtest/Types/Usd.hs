{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
module Backtest.Types.Usd where

import Backtest.Prelude
import Backtest.Types.Pct as Pct
import Data.Csv (FromField(..))
import Numeric (showFFloat)


data Fund a
  = Amt a
  | Bal a

data Stocks
data Bonds
data Total
data Withdrawal

-- | Total in pennies
newtype USD a = USD { totalCents :: Int }
  deriving (Eq, Ord, Num)

instance Semigroup (USD a) where
  a <> b = a + b

instance Monoid (USD a) where
  mempty = USD 0

instance FromField (USD a) where
  parseField f =
    fromFloat <$> parseField f

-- instance Num (USD a) where
--   (USD a) + (USD b) = USD $ a + b



-- show it rounded off
instance Show (USD a) where
  show m = mconcat
    [ "$"
    , sign $ totalCents m
    , show $ (abs $ totalCents m) `div` 100
    , "."
    , pad $ show $ abs $ cents m
    ]
    where pad [c] = ['0',c]
          pad x = x
          
          sign x
            | x < 0 = "-"
            | otherwise = ""

dollars :: USD a -> Int
dollars (USD c) = round $ fromIntegral c / 100

cents :: USD a -> Int
cents (USD c) = c `rem` 100

fromFloat :: Float -> USD a
fromFloat f = USD $ round (f * 100)

toFloat :: USD a -> Float
toFloat (USD c) = fromIntegral c / 100

fromCents :: Int -> USD a
fromCents = USD

-- | Convienence for making constants
--   dollars.cents
usd :: Float -> USD ass
usd f = fromFloat f
          
fromUSD :: USD a -> USD b
fromUSD (USD a) = USD a

minZero :: USD a -> USD a
minZero (USD n)
  | n >= 0 = USD n
  | otherwise = USD 0

-- | Ensures that the amt is treated as a loss
loss :: USD (Amt a) -> USD (Amt a)
loss (USD a) = USD (negate (abs a))

-- | Ensures that the amt is treated as a gain
gain :: USD (Amt a) -> USD (Amt a)
gain (USD a) = USD (abs a)

toAmount :: USD (Bal a) -> USD (Amt a)
toAmount = fromUSD

toBalance :: USD (Amt a) -> USD (Bal a)
toBalance = fromUSD

toBonds :: USD (Amt a) -> USD (Amt Bonds)
toBonds = fromUSD

toStocks :: USD (Amt a) -> USD (Amt Stocks)
toStocks = fromUSD

toWithdrawal :: USD (Amt a) -> USD (Amt Withdrawal)
toWithdrawal = fromUSD

toTotal :: USD (Amt a) -> USD (Amt Total)
toTotal = fromUSD



-- | the gain or loss from the start to the end
gains :: USD a -> USD a -> USD b
gains (USD s) (USD e) = USD $ abs e - abs s

-- | the absolute difference between two amounts
diff :: USD a -> USD a -> USD b
diff (USD a) (USD b) = USD $ abs (a - b)

avg :: USD a -> USD a -> USD a
avg a b = fromCents $ (totalCents a + totalCents b) `div` 2


gainsPercent :: USD a -> USD a -> Pct b
gainsPercent s e =
    Pct $ (fromIntegral e.totalCents) / (fromIntegral s.totalCents) - 1

-- | balances can never be zero, but returns can
balance :: Int -> USD (Bal a)
balance n = minZero (USD n)

-- | Applies a return to a balance
addToBalance :: USD (Amt a) -> USD (Bal b) -> USD (Bal b)
addToBalance (USD ret) (USD b) = balance $ b + ret

percentOf :: USD a -> USD (Bal b) -> Pct a
percentOf (USD a) (USD bal) = pctFromFloat (fromIntegral a / fromIntegral bal)

amount :: Pct a -> USD (Bal b) -> USD (Amt a)
amount p bal = fromFloat $
    (fromIntegral $ totalCents bal) * Pct.toFloat p / 100




newtype Millions = Millions Float
  deriving (Eq)

instance Show Millions where
  show (Millions f) = "$" <> showFFloat (Just 3) f "M"

millions :: USD a -> Millions
millions u = Millions $ fromIntegral (dollars u) / 1000000
