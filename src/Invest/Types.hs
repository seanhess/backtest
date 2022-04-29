{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Invest.Types where

import Invest.Prelude
import Data.Csv (FromNamedRecord(..), FromField, (.:))

newtype Year = Year Int
  deriving (Show, Eq, FromField)

newtype Percent = Percent Float
  deriving (Show, Eq, FromField)

data Returns = Returns
  { year :: Year
  , inflation :: Percent
  , totalStock :: Percent
  , totalBond :: Percent
  } deriving (Show, Eq, Generic)

instance FromNamedRecord Returns where
  parseNamedRecord m = do
    year       <- m .: "YEAR"
    inflation  <- m .: "CPI-U"
    totalBond  <- m .: "VBMFX"
    totalStock <- m .: "VTSMX"
    pure Returns {..}
    
