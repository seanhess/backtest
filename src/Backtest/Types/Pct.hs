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

fromPct :: Pct a -> Pct b
fromPct (Pct a) = Pct a

instance Eq (Pct a) where
  -- if they are equal to the nearest thousandth
  (Pct a) == (Pct b) =
    round (a * digits) == round (b * digits)
    where digits = 100000

instance Show (Pct a) where
  show p = showFFloat (Just 3) (toFloat p * 100) "%"

instance Semigroup (Pct a) where
  a <> b = a + b

instance Monoid (Pct a) where
  mempty = Pct 0


inverse :: Pct a -> Pct a
inverse (Pct a) = Pct (1 - a)

average :: [Pct a] -> Pct a
average [] = 0
average ps =
  Pct $ (toFloat $ sum ps) / fromIntegral (length ps)

compound :: Pct a -> Pct a -> Pct a
compound (Pct a) (Pct b) = Pct $ ((1+a) * (1+b)) - 1

diff :: Pct a -> Pct a -> Pct a
diff (Pct a) (Pct b) = Pct (abs $ b - a)
