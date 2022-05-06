{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , fromUSD
  , minZero
  , usd
  , HistoryRow(..)
  , History(..)
  , Portfolio(..), Balances, Changes
  , total
  , Amount, Stocks, Bonds, Withdrawal
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
  deriving (FromField, Num, Ord)


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
data USD a = USD { totalCents :: Int }
  deriving (Eq, Ord)

instance Semigroup (USD a) where
  (USD a) <> (USD b) = USD (a + b)

instance Monoid (USD a) where
  mempty = USD 0

dollars :: USD a -> Int
dollars (USD c) = round $ fromIntegral c / 100

cents :: USD a -> Int
cents (USD c) = c `rem` 100

fromFloat :: Float -> USD a
fromFloat f = USD $ round (f * 100)

instance FromField (USD a) where
  parseField f =
    fromFloat <$> parseField f

-- show it rounded off
instance Show (USD a) where
  show m = mconcat
    [ "$"
    , show $ totalCents m `div` 100
    , "."
    , pad $ show $ abs $ cents m
    ]
    where pad [c] = ['0',c]
          pad x = x
          



data Inflation
data Withdrawal
data Amount
data Stocks
data Bonds

data RealTotal

data HistoryRow = HistoryRow
  { year      :: Year
  , month     :: Int
  , stocks    :: USD Stocks
  , bonds     :: USD Bonds
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



data Portfolio stocks bonds = Portfolio
  { stocks :: USD stocks
  , bonds  :: USD bonds
  } deriving (Show, Eq)

type Balances = Portfolio Stocks Bonds
type Changes  = Portfolio Amount Amount 

instance (Semigroup stocks, Semigroup bonds) => Semigroup (Portfolio stocks bonds) where
  Portfolio s b <> Portfolio s' b' = Portfolio (s <> s') (b <> b')

-- instance Monoid (Portfolio stocks bonds) where


total :: Portfolio s b -> USD Amount
total b = USD $
    (totalCents b.stocks)
  + (totalCents b.bonds)




data YearResult = YearResult
  { history    :: History
  , start      :: Balances
  , end        :: Balances
  , returns    :: Changes
  , withdrawals :: USD Amount
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


gains ::  USD bal -> USD bal -> USD Amount
gains (USD s) (USD e) = USD $ e - s

gainsPercent :: USD bal -> USD bal -> Pct bal
gainsPercent s e =
    Pct $ (fromIntegral e.totalCents) / (fromIntegral s.totalCents) - 1

-- | Applies a return to a balance
addToBalance :: USD Amount -> USD bal -> USD bal
addToBalance (USD ret) (USD b) = balance $ b + ret

addAmounts :: USD Amount -> USD Amount -> USD Amount
addAmounts (USD a) (USD b) = USD $ a + b

loss :: USD amt -> USD amt
loss (USD a) = USD (negate (abs a))

gain :: USD amt -> USD amt
gain (USD a) = USD (abs a)

-- | balances can never be zero, but returns can
balance :: Int -> USD bal
balance n
  | n >= 0 = USD n
  | otherwise = USD 0

minZero :: USD a -> USD a
minZero (USD n) = balance n

amount :: Pct amt -> USD bal -> USD Amount
amount p bal = fromUSD $ amountBalance p bal

amountBalance :: Pct amt -> USD bal -> USD bal
amountBalance p bal = fromFloat $
    (fromIntegral $ totalCents bal) * toFloat p / 100

fromUSD :: USD a -> USD b
fromUSD (USD a) = USD a

-- dollars.cents
usd :: Float -> USD a
usd f = fromFloat f

toAmount :: USD a -> USD Amount
toAmount (USD a) = USD a