{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Invest.Types where

import Invest.Prelude
import Data.Csv (FromNamedRecord(..), FromField, (.:))

newtype Year = Year Int
  deriving (Show, Eq, FromField)

newtype Pct a = Pct Float
  deriving (Show, Eq, FromField)

data Inflation
data Return
data Balance
data Withdrawal

data Returns = Returns
  { year :: Year
  , inflation :: Pct Inflation
  , totalStock :: Pct Return
  , totalBond :: Pct Return
  } deriving (Show, Eq, Generic)

instance FromNamedRecord Returns where
  parseNamedRecord m = do
    year       <- m .: "YEAR"
    inflation  <- m .: "CPI-U"
    totalBond  <- m .: "VBMFX"
    totalStock <- m .: "VTSMX"
    pure Returns {..}
    

data YearResult = YearResult
  { start :: USD Balance
  , end :: USD Balance
  , growth :: USD Return
  , withdrawal :: USD Withdrawal
  , returns :: Returns
  } deriving (Show)

newtype USD a = USD {fromUSD :: Int}
  deriving (Show, Eq)

