{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Invest.Types
  ( Year(Year)
  , Pct(toFloat), pct
  , USD, dollars, cents, fromFloat
  , gainsPercent
  , addAmount
  , amount
  , gains
  , loss, gain
  , totalCents
  , fromUSD
  , balance
  , usd
  , HistoryRow(..)
  , History(..)
  , Portfolio(..), Balances, Changes
  , total
  , Amount, Stocks, Bonds
  , SimResult(..)
  , YearResult(..)
  )
  where

import Invest.Prelude
import Data.Text as Text (splitOn)
import Data.Csv (FromNamedRecord(..), FromField(..), (.:), Parser)
import Data.Decimal (Decimal, realFracToDecimal)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as Lazy
import Numeric (showFFloat)

newtype Year = Year Int
  deriving (Eq, FromField)


instance Show Year where 
  show (Year i) = show i

-- This can have strange errors
-- this is 60.4 %
newtype Pct a = Pct { toFloat :: Float }
  deriving (FromField)


digits :: Float
digits = 100000

instance Eq (Pct a) where
  -- if they are equal to the nearest thousandth
  (Pct a) == (Pct b) =
    round (a * digits) == round (b * digits)

instance Show (Pct a) where
  show p = showFFloat (Just 2) (toFloat p) ""


-- 100x float
pct :: Float -> Pct a
pct f = Pct (f / 100)


-- | Total in pennies
data USD a = USD { totalCents :: Int }
  deriving (Eq)

instance Semigroup (USD a) where
  (USD a) <> (USD b) = USD (a + b)

instance Monoid (USD a) where
  mempty = USD 0

dollars :: USD a -> Int
dollars (USD c) = c `div` 100

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
    , show $ dollars m
    , "."
    , pad $ show $ abs $ cents m
    ]
    where pad [c] = ['0',c]
          pad x = x
          



data Inflation
data Withdrawal
data Amount
-- data Balance
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
addAmount :: USD Amount -> USD bal -> USD bal
addAmount (USD ret) (USD b) = balance $ b + ret

loss :: USD Amount -> USD Amount
loss (USD a) = USD (negate (abs a))

gain :: USD Amount -> USD Amount
gain (USD a) = USD (abs a)

-- | balances can never be zero, but returns can
balance :: Int -> USD bal
balance n
  | n >= 0 = USD n
  | otherwise = USD 0

amount :: Pct amt -> USD bal -> USD Amount
amount p bal = fromFloat $
    (fromIntegral $ totalCents bal) * toFloat p / 100

fromUSD :: USD a -> USD b
fromUSD (USD a) = USD a

-- dollars.cents
usd :: Float -> USD a
usd f = fromFloat f