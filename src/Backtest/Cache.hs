module Backtest.Cache where

import Backtest.Prelude
import Backtest.Types


-- save to a csv file?
type SWRCache = Map (Pct Stocks, NumYears) (Pct Withdrawal)


-- data CacheRow = CacheRow
--   { s100 :: Pct Withdrawal
--   , s90 :: Pct Withdrawal
--   , s80 :: Pct Withdrawal
--   , s70 :: Pct Withdrawal
--   , s60 :: Pct Withdrawal
--   , s50 :: Pct Withdrawal
--   }

-- buildSWRCache :: SWRCache
-- buildSWRCache = _

-- saveToFile :: MonadIO m => FilePath -> SWRCache -> m ()
-- saveToFile = _

-- loadFromFile :: MonadIO m => FilePath -> m SWRCache
-- loadFromFile = _

-- lookup :: Pct Stocks -> NumYears -> Maybe (Pct Withdrawal)
-- lookup = _

