module Backtest.History where

import Backtest.Prelude
import Backtest.Types
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
toHistories hr = zipWith toHistory hr (drop 1 hr)


toHistory :: HistoryRow -> HistoryRow -> History
toHistory past now =
    History
      { year = now.year
      , stocks = gainsPercent past.stocks now.stocks
      , bonds = gainsPercent past.bonds now.bonds
      }


samples :: Int -> [History] -> [[History]]
samples years hs = List.tails hs
  & fmap (take years)
  & filter (\hs' -> length hs' >= years)
