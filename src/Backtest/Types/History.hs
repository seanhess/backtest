{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Types.History where


import Backtest.Prelude
import Backtest.Types.Usd
import Backtest.Types.Pct
import Data.ByteString.Lazy (ByteString)
import Data.Csv (FromNamedRecord(..), FromField(..), (.:), Parser)

newtype Year = Year Int
  deriving (Eq, FromField, Ord)

instance Show Year where 
  show (Year i) = show i

data Inflation
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
