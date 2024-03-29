module Backtest.History where

import Backtest.Prelude
import Backtest.Types
import Backtest.Types.Pct as Pct
import Data.ByteString.Lazy (readFile)
import Data.Csv as Csv (decodeByName, Header)
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Vector as Vector (Vector, toList)


loadHistories :: IO (NonEmpty History)
loadHistories = toHistories <$> loadReturns


loadReturns :: IO (NonEmpty HistoryRow)
loadReturns = do
    putStrLn "Loading"
    f <- readFile "data/data.csv"
    case Csv.decodeByName f of
        Left e -> fail $ "Decoding CSV Failed: " <> e
        Right (_, rs) -> do
            case nonEmpty $ filter firstMonth $ Vector.toList rs of
              Just ne -> pure ne
              Nothing -> fail "loadReturns, no data rows found"
              

    where
        firstMonth rets = rets.month == 1


toHistories :: NonEmpty HistoryRow -> NonEmpty History
toHistories hr =
  let hs = catMaybes $ zipWith toHistory (NE.toList hr) (drop 1 (NE.toList hr))
  in case nonEmpty hs of
    Nothing -> error "toHistories: no histories found from rows"
    Just x -> x



fakeHistory :: Year -> History
fakeHistory y = History y (Portfolio (pct 0) (pct 0)) (Portfolio (usd 4582) (usd 46.65)) (CAPE 30)

-- find the history or use a fake one
simHistory :: [History] -> Year -> History
simHistory hs y =
  fromMaybe (fakeHistory y) $ List.find (\h -> h.year == y) hs

simHistories :: NumYears -> [History] -> [History]
simHistories _ [] = []
simHistories (NumYears yl) hs@(h:_) =
  let (Year s) = h.year
      end = Year $ s + yl - 1

  -- maximum of 10 sim histories
  in map (simHistory hs) [h.year..end]

toHistory :: HistoryRow -> HistoryRow -> Maybe History
toHistory past now = do
    c <- now.cape
    pure $ History
      { year = now.year
      , returns = Portfolio 
          { stocks = gainsPercent past.stocks now.stocks
          , bonds = gainsPercent past.bonds now.bonds
          }
      , values = Portfolio
          { stocks = now.stocks
          , bonds = now.bonds
          }
      , cape = c
      }


-- it's a nonempty of lists
samples :: NumYears -> NonEmpty History -> NonEmpty (NonEmpty History)
samples years hs = NE.tails hs
  & fmap (take (fromNumYears years))
  & fmap (simHistories years)
  & NE.filter isNumYears
  & map (NE.fromList)
  & filter isGoodYearRange
  & NE.fromList
  where
    isNumYears :: [History] -> Bool
    isNumYears hs' =
      length hs' == fromNumYears years

    isGoodYearRange :: NonEmpty History -> Bool
    isGoodYearRange hs' =
      (head hs').year <= (Year 2020) && (last hs').year <= (Year 2030)






-- Trying to predict SHORT term returns / crashes
data Crash = Crash
  { start :: Year
  , depth :: Pct Stocks
  , years :: [History]
  , cape :: CAPE
  , balance :: USD (Bal Stocks)

  -- priorReturns
  , prior1y :: Pct (Return Stocks)
  , prior2y :: Pct (Return Stocks)
  , prior3y :: Pct (Return Stocks)
  , prior4y :: Pct (Return Stocks)
  , prior5y :: Pct (Return Stocks)
  } deriving (Show)


-- -- | All the years that are low
-- crashes :: [(HistoryRow, History)] -> [History]
-- crashes hs = fromMaybe [] $ do
--   (now, first) <- headMay hs
--   let start = first.values.stocks
--   pure $ filter (\h -> h.values.stocks < start) hs

-- crashDepth :: History -> [History] -> Pct Stocks
-- crashDepth start hs =
--   fromPct $ Pct.inverse $ percentOf (minimum $ map (\h -> h.values.stocks) hs) start.values.stocks

-- crashInfo :: [History] -> History -> [History] -> Maybe Crash
-- crashInfo _ _ []        = Nothing
-- crashInfo hs start low = Just $ Crash
--   { start = start.year
--   , depth = crashDepth start low
--   , years = low
--   , cape = start.cape
--   , balance = start.values.stocks
--   -- , priorReturns = _
--   , prior1y = compoundStockReturn $ take 1 $ priorYears start.year hs
--   , prior2y = compoundStockReturn $ take 2 $ priorYears start.year hs
--   , prior3y = compoundStockReturn $ take 3 $ priorYears start.year hs
--   , prior4y = compoundStockReturn $ take 4 $ priorYears start.year hs
--   , prior5y = compoundStockReturn $ take 5 $ priorYears start.year hs
--   }

-- it's more of a fold I think
compoundStockReturn :: [History] -> Pct (Return Stocks)
compoundStockReturn hs = foldl Pct.compound (pct 0) $ map (\h -> h.returns.stocks) hs


priorYears :: Year -> NonEmpty History -> [History]
priorYears y hs = List.reverse $ takeWhile (\h -> h.year < y) $ NE.toList hs


historicalReturns :: Balances -> [History] -> [Pct (Return Total)]
historicalReturns bal hs =
  let ps = pctStocks bal
      pb = pctBonds ps
  in map (ret ps pb) hs
    where
      ret :: Pct Stocks -> Pct Bonds -> History -> Pct (Return Total)
      ret ps pb h = totalReturn
        [ weightedReturn ps (fromPct h.returns.stocks)
        , weightedReturn pb (fromPct h.returns.bonds)
        ]
