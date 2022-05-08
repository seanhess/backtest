{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Types.Pct where

import Backtest.Prelude
import Numeric (showFFloat)
import Data.Csv (FromNamedRecord(..), FromField(..), (.:), Parser)


-- this is 60.4 %
newtype Pct a = Pct { toFloat :: Float }
  deriving (FromField, Num, Ord, Fractional)

-- 100x float
pct :: Float -> Pct a
pct f = Pct (f / 100)

pctFromFloat :: Float -> Pct a
pctFromFloat = Pct

instance Eq (Pct a) where
  -- if they are equal to the nearest thousandth
  (Pct a) == (Pct b) =
    round (a * digits) == round (b * digits)
    where digits = 100000

instance Show (Pct a) where
  show p = showFFloat (Just 3) (toFloat p * 100) "%"



