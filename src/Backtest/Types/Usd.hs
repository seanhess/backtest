{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
module Backtest.Types.Usd where

import Backtest.Prelude
import Data.Csv (FromField(..))
import Numeric (showFFloat)

data Funds
  = Amt
  | Bal

data Asset
  = Stocks
  | Bonds
  | Total
  | Withdrawal

-- | Total in pennies
data USD (at :: Funds) (asset :: Asset) = USD { totalCents :: Int }
  deriving (Eq, Ord)

instance Semigroup (USD f a) where
  (USD a) <> (USD b) = USD (a + b)

instance Monoid (USD f a) where
  mempty = USD 0

dollars :: USD f a -> Int
dollars (USD c) = round $ fromIntegral c / 100

cents :: USD f a -> Int
cents (USD c) = c `rem` 100

fromFloat :: Float -> USD f a
fromFloat f = USD $ round (f * 100)

toFloat :: USD f a -> Float
toFloat (USD c) = fromIntegral c / 100

fromCents :: Int -> USD f a
fromCents = USD

instance FromField (USD f a) where
  parseField f =
    fromFloat <$> parseField f

-- show it rounded off
instance Show (USD f a) where
  show m = mconcat
    [ "$"
    , show $ totalCents m `div` 100
    , "."
    , pad $ show $ abs $ cents m
    ]
    where pad [c] = ['0',c]
          pad x = x

-- | Convienence for making constants
--   dollars.cents
usd :: Float -> USD f ass
usd f = fromFloat f
          
fromUSD :: USD f a -> USD f' b
fromUSD (USD a) = USD a

minZero :: USD f a -> USD f a
minZero (USD n)
  | n >= 0 = USD n
  | otherwise = USD 0

loss :: USD Amt a -> USD Amt a
loss (USD a) = USD (negate (abs a))

gain :: USD Amt a -> USD Amt a
gain (USD a) = USD (abs a)

toAmount :: USD Bal a -> USD Amt a
toAmount = fromUSD

toBonds :: USD f a -> USD f Bonds
toBonds = fromUSD

toStocks :: USD f a -> USD f Stocks
toStocks = fromUSD

toWithdrawal :: USD f a -> USD f Withdrawal
toWithdrawal = fromUSD

toTotal :: USD f a -> USD f Total
toTotal = fromUSD



newtype Millions = Millions Float
  deriving (Eq)

instance Show Millions where
  show (Millions f) = "$" <> showFFloat (Just 3) f "M"

millions :: USD f a -> Millions
millions u = Millions $ fromIntegral (dollars u) / 1000000
