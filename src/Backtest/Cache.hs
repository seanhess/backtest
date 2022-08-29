{-# LANGUAGE OverloadedLists #-}
module Backtest.Cache where

import Backtest.Prelude hiding (lookup)
import Backtest.Types
import Data.Csv (ToNamedRecord(..), (.=), (.:), namedRecord, FromNamedRecord(..), Parser, encodeDefaultOrderedByName, DefaultOrdered(..), decodeByName)
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!), Map)
import qualified Backtest.Types.Pct as Pct
import Control.Monad (guard, zipWithM)
import qualified Data.List as List
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Backtest.MSWR as MSWR
import Backtest.Optimize (maximizeRate, OptimizeResult(..))
import Backtest.Strategy (rebalanceFixed, thousand)
import Backtest.History (samples)
import Backtest.Simulation (rebalance, balances, yearsLeft, yearsElapsed, Actions, withdraw, npvExpenses, lastWithdrawal)
import Backtest.Debug (dump, debug, printTable, Column(..))
import Numeric (showFFloat)



-- ok!
-- just run all of them

-- guaranteed to be the right length and have an entry for every allocation
type SWRCache = KeyRange NumYears (KeyRange Allocation (Pct Withdrawal))

lookup :: Allocation -> NumYears -> SWRCache -> Pct Withdrawal
lookup al ny cache =
  cache & lookupKey ny & lookupKey al





cachedWithdrawal :: SWRCache -> Allocation -> NumYears -> NumYears -> [Transaction Expense] -> Balances -> USD (Amt Withdrawal) -> USD (Amt Withdrawal)
cachedWithdrawal cache alloc yl ye exps bal old = 
  let swr = lookup alloc yl cache
      expNPV = npvExpenses ye exps
      tot = total bal - toBalance (toTotal expNPV)
      new = amount swr tot
  in max old new

withdrawCached :: SWRCache -> Allocation -> [Transaction Expense] -> USD (Amt Withdrawal) -> Actions ()
withdrawCached cache alloc exps start = do
  old <- fromMaybe start <$> lastWithdrawal
  bal <- balances
  yl <- yearsLeft
  ye <- yearsElapsed
  -- let wda = dump "withdrawCached" (start, old, ny) $ cachedWithdrawal cache alloc ny exps bal old
  let wda = cachedWithdrawal cache alloc yl ye exps bal old
  withdraw wda
  pure ()


calculateAllMSWRs :: NonEmpty History -> [(NumYears, [(Allocation, Pct Withdrawal)])]
calculateAllMSWRs hs =
  map runAllNumYears [minBound..maxBound]

  where
    runAllNumYears :: NumYears -> (NumYears, [(Allocation, Pct Withdrawal)])
    runAllNumYears ny = 
      (ny, mapMaybe (runOptimize ny) [minBound..maxBound])

    runOptimize :: NumYears -> Allocation -> Maybe (Allocation, Pct Withdrawal)
    runOptimize (NumYears 1) al = do
      dump "Optimize" (1, al, pct 100) $ Just (al, pct 100)

    runOptimize ny al = do
      let ss = samples ny hs
      let bal = thousand al
      let steps = maximizeRate isSuccess (MSWR.runRate ss bal (rebalance (rebalanceFixed al)))
      OptimizeResult pw _ <- lastMay $ steps
      pure $ dump "Optimize" (fromNumYears ny, al, pw) $ (al, pw)

    isSuccess :: NonEmpty SimResult -> Bool
    isSuccess srs = MSWR.successRate srs == pct 100




-- | Build a cache from a list of numrows, etc
buildSWRCache :: [(NumYears, [(Allocation, Pct Withdrawal)])] -> Either String SWRCache
buildSWRCache nyap = do
  rows <- mapM eachYear nyap
  keyRange $ zip (map fst nyap) rows

  where
    eachYear :: (NumYears, [(Allocation, Pct Withdrawal)]) -> Either String (KeyRange Allocation (Pct Withdrawal))
    eachYear (ny, apws) =
      case keyRange apws of
        Left e -> Left $ "Failed: " <> (show $ fromNumYears ny) <> " - " <> e
        Right kr -> pure kr


testCacheInput :: [(NumYears, [(Allocation, Pct Withdrawal)])]
testCacheInput = 
  map (\ny -> (ny, map (\al -> (al, pct 5)) [minBound..maxBound])) [minBound..maxBound]

testCache :: SWRCache
testCache =
  case buildSWRCache $ testCacheInput of
    Left e -> error $ "Failed build cache: " <> e
    Right c -> c

dumpCache :: SWRCache -> IO ()
dumpCache c =
  flip printTable (toCacheRows c) $ 
    (Column "year" 7 $ \r -> show (fromNumYears r.years)) : map col [minBound..maxBound]
  where
    col al = Column (show al) 8 $ \r -> show (lookupKey al r.swrs)



-- | save it to a file!
saveToFile :: MonadIO m => FilePath -> SWRCache -> m ()
saveToFile fp cache = do
  let bs = encodeDefaultOrderedByName $ toCacheRows cache
  liftIO $ LBS.writeFile fp bs

loadFromFile :: (MonadIO m, MonadFail m) => FilePath -> m SWRCache
loadFromFile fp = do
  bs <- liftIO $ LBS.readFile fp
  let (_, vrs) = getRight (decodeByName bs)
  let cache = getRight $ fromCacheRows $ Vector.toList vrs
  pure cache
  where
    getRight e =
      case e of
        Left err -> error $ "Failed load: " <> err
        Right val -> val


data CacheRow = CacheRow
  { years :: NumYears
  , swrs :: KeyRange Allocation (Pct Withdrawal)
  }

toCacheRows :: SWRCache -> [CacheRow]
toCacheRows cache =
  map cacheRow $ toListRange cache
  where
    cacheRow (ny, ar) = CacheRow ny ar

fromCacheRows :: [CacheRow] -> Either String SWRCache
fromCacheRows crs = do
  keyRange $ map fromCacheRow crs
  where
    fromCacheRow (CacheRow y wrs) = (y, wrs)


instance DefaultOrdered CacheRow where
  headerOrder _ =
    let als = [minBound .. maxBound] :: Vector Allocation
    in ["years"] <> fmap (cs . show) als


instance ToNamedRecord CacheRow where
  toNamedRecord (CacheRow ny kr) =
    let rows = map eachAlloc $ toListRange kr
    in namedRecord $ ["years" .= fromNumYears ny] <> rows
    where
      eachAlloc :: (Allocation, Pct Withdrawal) -> (BS.ByteString, BS.ByteString)
      eachAlloc (a, p) =
        cs (show a) .= (showFFloat (Just 2) ((Pct.toFloat p) * 100) "")

instance FromNamedRecord CacheRow where
  parseNamedRecord m = do
    ny <- m .: "years" :: Parser Int
    Right kr <- keyRange <$> mapM parseAlloc [minBound..maxBound] 
    pure $ CacheRow (numYears ny) kr

    where
      parseAlloc :: Allocation -> Parser (Allocation, Pct Withdrawal)
      parseAlloc al = do
        p <- m .: (cs $ show al)
        pure (al, pct p)





-- | A complete collection for bounded keys
newtype KeyRange key val = KeyRange (Map key val)
  deriving (Show, Eq)

-- | Only constructs a key range if all the keys are present
keyRange :: forall key val. (Show key, Bounded key, Enum key, Eq key, Ord key) => [(key, val)] -> Either String (KeyRange key val)
keyRange [] = Left "keyRange: No input"
keyRange input = do
  kvs <- zipWithM matchKey [minBound..maxBound] $ List.sortOn fst input
  pure $ KeyRange $ Map.fromList kvs
  where
    matchKey :: key -> (key, val) -> Either String (key, val)
    matchKey k1 (k2, val) = do
      if k1 == k2
        then pure (k1, val)
        else Left $ "Missing key: " <> show k1

lookupKey :: (Bounded key, Ord key) => key -> KeyRange key val -> val
lookupKey k (KeyRange kr) = kr ! k

toListRange :: KeyRange key val -> [(key, val)]
toListRange (KeyRange m) = Map.toList m
