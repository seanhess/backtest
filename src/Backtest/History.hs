module Backtest.History where

import Backtest.Prelude
import Backtest.Types
import Backtest.Types.Pct as Pct
import Data.ByteString.Lazy (readFile)
import Data.Csv as Csv (decodeByName, Header)
import Data.List as List
import Data.Vector as Vector (Vector, toList)


loadReturns :: IO [HistoryRow]
loadReturns = do
    putStrLn "Loading"
    f <- readFile "data/data.csv"
    case Csv.decodeByName f of
        Left e -> fail $ "Decoding CSV Failed: " <> e
        Right (_, rs) -> do
            pure $ filter firstMonth $ Vector.toList rs
    where
        firstMonth rets = rets.month == 1


toHistories :: [HistoryRow] -> [History]
toHistories hr = catMaybes $ zipWith toHistory hr (drop 1 hr)


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


samples :: Int -> [History] -> [[History]]
samples years hs = List.tails hs
  & fmap (take years)
  & filter (\hs' -> length hs' >= years)




-- Trying to predict SHORT term returns / crashes
data Crash = Crash
  { start :: Year
  , depth :: Pct Stocks
  , years :: [History]
  , cape :: CAPE
  , balance :: USD (Bal Stocks)

  -- priorReturns
  -- , return1y :: Pct (Return Stocks)
  -- , return2y :: Pct (Return Stocks)
  -- , return3y :: Pct (Return Stocks)
  -- , return4y :: Pct (Return Stocks)
  -- , return5y :: Pct (Return Stocks)

  } deriving (Show)


-- | All the years that are low
crashes :: [History] -> [History]
crashes hs = fromMaybe [] $ do
  first <- headMay hs
  let start = first.values.stocks
  pure $ filter (\h -> h.values.stocks < start) hs

crashDepth :: History -> [History] -> Pct Stocks
crashDepth start hs =
  fromPct $ Pct.inverse $ percentOf (minimum $ map (\h -> h.values.stocks) hs) start.values.stocks

crashInfo :: History -> [History] -> Maybe Crash
crashInfo _ []      = Nothing
crashInfo start low = Just $ Crash
  { start = start.year
  , depth = crashDepth start low
  , years = low
  , cape = start.cape
  , balance = start.values.stocks
  -- , priorReturns = _
  }




historicalReturns :: Balances -> [History] -> [Pct (Return Total)]
historicalReturns bal hs =
  let ps = allocationStocks bal
      pb = pctBonds ps
  in map (ret ps pb) hs
    where
      ret :: Pct Stocks -> Pct Bonds -> History -> Pct (Return Total)
      ret ps pb h = totalReturn
        [ weightedReturn ps (fromPct h.returns.stocks)
        , weightedReturn pb (fromPct h.returns.bonds)
        ]



