{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Invest.Types where

import Invest.Prelude
import Data.Csv (FromNamedRecord(..), FromField(..), (.:), Parser)
import Data.Decimal (Decimal, realFracToDecimal)

newtype Year = Year Int
  deriving (Eq, FromField)

instance Show Year where 
  show (Year i) = show i

newtype Pct a = Pct Decimal
  deriving (Eq)

instance FromField (Pct a) where
  parseField field = do
    f <- parseField field :: Parser Float
    pure $ Pct $ realFracToDecimal 2 f


instance Show (Pct a) where
  show (Pct p) = show p <> "%"

newtype USD a = USD { fromUSD :: Int }
  deriving (Eq)

instance Show (USD a) where
  show (USD d) = '$' : show d



data Inflation
data Withdrawal
data Return
data Change
data Balance

data Real
data Nom

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
  { year        :: Year
  , start       :: USD Balance
  , realEnd     :: USD Balance
  , realChange  :: Pct Change
  , investments :: Pct Return
  , withdrawal  :: Pct Withdrawal
  , inflation   :: Pct Inflation
  } deriving (Show)


data SimResult = SimResult
  { startYear :: Year
  , startBalance :: USD Balance
  , endYear :: Year
  , endBalance :: USD Balance
  , years :: [YearResult]
  }