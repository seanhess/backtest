{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
module Backtest.Types
  ( Year(Year)
  , Pct(toFloat), pct, pctFromFloat
  , USD, dollars, cents, fromFloat
  , gainsPercent
  , addToBalance
  , addAmounts
  , amount
  , amountBalance
  , toAmount
  , gains
  , loss, gain
  , totalCents
  -- , fromUSD
  , minZero
  , usd
  , percentOf
  , toStocks
  , toBonds
  , toWithdrawal
  , toTotal
  , HistoryRow(..)
  , History(..)
  , Portfolio(..), Balances, Changes
  , total
  , Funds(..)
  , Asset(..)
  , SimResult(..)
  , YearResult(..)
  , Success
  , RateResult(..)
  )
  where

import Backtest.Prelude
import Data.Text as Text (splitOn)
import Data.Csv (FromNamedRecord(..), FromField(..), (.:), Parser)
import Data.Decimal (Decimal, realFracToDecimal)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Numeric (showFFloat)

newtype Year = Year Int
  deriving (Eq, FromField, Ord)


instance Show Year where 
  show (Year i) = show i

-- This can have strange errors
-- this is 60.4 %
newtype Pct a = Pct { toFloat :: Float }
  deriving (FromField, Num, Ord, Fractional)


pctFromFloat :: Float -> Pct a
pctFromFloat = Pct

digits :: Float
digits = 100000

instance Eq (Pct a) where
  -- if they are equal to the nearest thousandth
  (Pct a) == (Pct b) =
    round (a * digits) == round (b * digits)

instance Show (Pct a) where
  show p = showFFloat (Just 3) (toFloat p * 100) "%"


-- 100x float
pct :: Float -> Pct a
pct f = Pct (f / 100)


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
          

data Inflation

data Funds
  = Amt
  | Bal

data Asset
  = Stocks
  | Bonds
  | Total
  | Withdrawal


data RealTotal

data HistoryRow = HistoryRow
  { year      :: Year
  , month     :: Int
  , stocks    :: USD Bal Stocks
  , bonds     :: USD Bal Bonds
  , cpi       :: Pct Inflation
  } deriving (Show, Eq)

instance FromNamedRecord HistoryRow where
  parseNamedRecord m = do
    date   <- m .: "Date" :: Parser Float
    cpi    <- m .: "CPI"
    stocks <- clean =<< m .: "Real Total Return Price"
    bonds  <- clean =<< m .: "Real Total Bond Returns"

    let yr = round date :: Int
    let month = (round $ (date - (fromIntegral yr)) * 100) :: Int
    let year = Year yr
        
    pure HistoryRow {..}

clean :: FromField a => ByteString -> Parser a
clean s = do
  parseField $ cs $ filter (/= ',') $ cs s


data History = History
  { year      :: Year
  , stocks    :: Pct Stocks
  , bonds     :: Pct Bonds
  } deriving (Show, Eq)

-- | Defines rebalancing rules and percentages, also withdrawal strategies?
-- we need to offload this to the user
-- data Portfolio = Portfolio



data Portfolio f = Portfolio
  { stocks :: USD f Stocks
  , bonds  :: USD f Bonds
  } deriving (Show, Eq)

type Balances = Portfolio Bal
type Changes  = Portfolio Amt

instance Semigroup (Portfolio f) where
  Portfolio s b <> Portfolio s' b' = Portfolio (s <> s') (b <> b')

-- instance Monoid (Portfolio stocks bonds) where


total :: Portfolio f -> USD f Total
total b = USD $
    (totalCents b.stocks)
  + (totalCents b.bonds)




data YearResult = YearResult
  { history    :: History
  , start      :: Balances
  , end        :: Balances
  , returns    :: Changes
  , withdrawals :: USD Amt Withdrawal
  , actions    :: Changes
  -- , rebalance :: Changes
  -- , cpi        :: Pct Inflation
  } deriving (Show)



data SimResult = SimResult
  { startYear :: Year
  , startBalance :: Balances
  , endYear :: Year
  , endBalance :: Balances
  , years :: [YearResult]
  } deriving (Show)


data Success

data RateResult = RateResult
  { years :: Int
  , rate :: Pct Withdrawal
  , success :: Pct Success
  } deriving (Show)


-- 1. We combine percentages in a Change. They must be added.
-- 2. Changes can only be applied once

-- loss :: USD amt -> USD Balance -> USD Balance
-- loss (USD amt) (USD bal) = USD (bal - amt)

-- gain :: USD amt -> USD Balance -> USD Balance
-- gain (USD amt) (USD bal) = USD (bal + amt)

--  can we simply add it?


-- loss :: USD a -> USD a
-- loss (USD a) = USD (-a)

-- ($+) :: USD a -> USD b -> USD c
-- (USD a) $+ (USD b) = USD $ a + b

-- ($-) :: USD a -> USD a -> USD Return
-- ($-) (USD a) (USD b) = USD $ b - a

-- compound :: Pct a -> Pct b -> Pct c
-- compound (Pc )= 


gains :: USD f a -> USD f a -> USD Amt a
gains (USD s) (USD e) = USD $ e - s

gainsPercent :: USD f a -> USD f a -> Pct a
gainsPercent s e =
    Pct $ (fromIntegral e.totalCents) / (fromIntegral s.totalCents) - 1

-- | Applies a return to a balance
addToBalance :: USD Amt b -> USD Bal b -> USD Bal b
addToBalance (USD ret) (USD b) = balance $ b + ret

addAmounts :: USD Amt a -> USD Amt a -> USD Amt a
addAmounts (USD a) (USD b) = USD $ a + b

loss :: USD Amt a -> USD Amt a
loss (USD a) = USD (negate (abs a))

gain :: USD Amt a -> USD Amt a
gain (USD a) = USD (abs a)

toBonds :: USD f a -> USD f Bonds
toBonds = fromUSD

toStocks :: USD f a -> USD f Stocks
toStocks = fromUSD

toWithdrawal :: USD f a -> USD f Withdrawal
toWithdrawal = fromUSD

toTotal :: USD f a -> USD f Total
toTotal = fromUSD


percentOf :: USD f a -> USD Bal b -> Pct a
percentOf (USD a) (USD bal) = pctFromFloat (fromIntegral a / fromIntegral bal)

-- | balances can never be zero, but returns can
balance :: Int -> USD Bal a
balance n = minZero (USD n)

minZero :: USD f a -> USD f a
minZero (USD n)
  | n >= 0 = USD n
  | otherwise = USD 0


amount :: Pct ass -> USD Bal b -> USD Amt ass
amount p bal = fromFloat $
    (fromIntegral $ totalCents bal) * toFloat p / 100

amountBalance :: Pct ass -> USD Bal b -> USD Bal ass
amountBalance p bal = fromUSD $ amount p bal

amountWithdrawal :: Pct Withdrawal -> USD Bal b -> USD Amt Withdrawal
amountWithdrawal p bal = fromUSD $ amount p bal


fromUSD :: USD f a -> USD f' b
fromUSD (USD a) = USD a

-- | Convienence for making constants
--   dollars.cents
usd :: Float -> USD f ass
usd f = fromFloat f

toAmount :: USD Bal a -> USD Amt a
toAmount = fromUSD