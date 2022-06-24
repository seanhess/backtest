{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Types.History where


import Backtest.Prelude
import Backtest.Types.Usd
import Backtest.Types.Pct
import Backtest.Types.Portfolio
import Backtest.Types.Return
import Data.ByteString.Lazy (ByteString)
import Data.Csv (FromNamedRecord(..), FromField(..), (.:), Parser)
import Text.Read (readMaybe)
import Data.Either (fromRight)

-- must be between 0-70
newtype NumYears = NumYears { fromNumYears :: Int }
  deriving (Eq, Ord, Show)

instance Enum NumYears where
  toEnum = numYears
  fromEnum = fromNumYears

instance Bounded NumYears where
  minBound = NumYears 1
  maxBound = NumYears 60

numYears :: Int -> NumYears
numYears n
  | n < (fromNumYears minBound) = minBound
  | n > (fromNumYears maxBound) = maxBound
  | otherwise = NumYears n

newtype Year = Year { fromYear :: Int }
  deriving (Eq, FromField, Ord, Enum, Num)

nextYear :: Year -> Year
nextYear (Year n) = Year (n+1)

instance Show Year where 
  show (Year i) = show i

data Inflation
data RealTotal

data CAPE = CAPE { fromCAPE :: Float }
  deriving (Eq, Read, Ord)

instance Show CAPE where
  show (CAPE c) = show c


-- instance FromField CAPE where
--   parseField bs = do
--     s <- parseField bs
--     case readMaybe s of
--       "NA" -> pure NA
--       _ -> pure $ CAPE (read s)

-- Performance of a particular year
data HistoryRow = HistoryRow
  { year      :: Year
  , month     :: Int
  , stocks    :: USD (Bal Stocks)
  , bonds     :: USD (Bal Bonds)
  , cape      :: Maybe CAPE
  } deriving (Show, Eq)

instance FromNamedRecord HistoryRow where
  parseNamedRecord m = do
    date   <- m .: "Date" :: Parser Float
    -- cpi    <- m .: "CPI"
    capes  <- m .: "CAPE"

    -- you need total return to account for dividends too
    stocks <- clean =<< m .: "Real Total Return Price"
    bonds  <- clean =<< m .: "Real Total Bond Returns"

    let yr = round date :: Int
    let month = (round $ (date - (fromIntegral yr)) * 100) :: Int
    let year = Year yr

    let cape = CAPE <$> readMaybe capes
        
    pure HistoryRow {..}

clean :: FromField a => ByteString -> Parser a
clean s = do
  parseField $ cs $ filter (/= ',') $ cs s


-- Performance of the PREVIOUS year
data History = History
  { year      :: Year
  , returns   :: Portfolio Pct Return
  , values    :: Portfolio USD Bal
  , cape      :: CAPE
  } deriving (Show)

