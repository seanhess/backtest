module Backtest.Debug where

import Backtest.Types
import Backtest.Prelude
import Debug.Trace
import Data.List as List

import qualified Data.List.NonEmpty as NE


printYears :: [YearStart] -> IO ()
printYears ys = do
  printYearHeader
  mapM_ printYear ys

printYearHeader :: IO ()
printYearHeader =
    printTableRow 9
      [ "Year"
      , "ret.stck"
      , "ret.bnds"
      , "beg.stck"
      , "beg.bnds"
      , "income"
      , "expenses"
      , "withdraw"
      , "act.stck"
      , "act.bnds"
      , "end.stck"
      , "end.bnds"
      , "cape"
      ]

printYear :: YearStart -> IO ()
printYear yr =
    printTableRow 9
      [ show yr.history.year
      , show yr.returns.stocks
      , show yr.returns.bonds
      , show yr.start.stocks
      , show yr.start.bonds
      , show yr.netIncome
      , show yr.netExpenses
      , show yr.withdrawal
      , show yr.actions.stocks
      , show yr.actions.bonds
      , show yr.end.stocks
      , show yr.end.bonds
      , show yr.history.cape.fromCAPE
      ]

printWithdrawalResults :: WithdrawalResults -> IO ()
printWithdrawalResults wr = do
    putStrLn $ "  low: " <> show wr.low
    putStrLn $ "  p10: " <> show wr.p10
    putStrLn $ "  p25: " <> show wr.p25
    putStrLn $ "  med: " <> show wr.med
    putStrLn $ "  p75: " <> show wr.p75
    putStrLn $ "  p90: " <> show wr.p90

printWithdrawalSpread :: Show a => WithdrawalSpread a -> IO ()
printWithdrawalSpread wr = do
    putStrLn $ " low%: " <> show wr.wlow
    putStrLn $ " 2.0%: " <> show wr.w2_0
    putStrLn $ " 2.5%: " <> show wr.w2_5
    putStrLn $ " 3.0%: " <> show wr.w3_0
    putStrLn $ " 3.5%: " <> show wr.w3_5
    putStrLn $ " 4.0%: " <> show wr.w4_0
    putStrLn $ " 4.5%: " <> show wr.w4_5
    putStrLn $ "high%: " <> show wr.whigh




printTableRow :: Int -> [String] -> IO ()
printTableRow p items = do
    putStrLn $ "|" <> (List.intercalate " |" $ map (padLeft p) items) <> " |"


withdrawalResultsCols :: [Column WithdrawalResults]
withdrawalResultsCols =
    [ Column "low" 8  $ show . (.low)
    , Column "p10" 8  $ show . (.p10)
    , Column "p25" 8 $ show . (.p25)
    , Column "med" 8 $ show . (.med)
    , Column "p75" 8 $ show . (.p75)
    , Column "p90" 8 $ show . (.p90)
    ]

padLeft :: Int -> String -> String
padLeft n s
    | length s < n = padLeft n (' ':s)
    | otherwise = s



data Column a = Column
  { label :: String
  , size :: Int
  , value :: (a -> String)
  }


printTable :: [Column a] -> [a] -> IO ()
printTable cols as = mapM_ putStrLn $ tableRows cols as

tableRows :: [Column a] -> [a] -> [String]
tableRows cols as = headerRow cols : map (tableRow cols) as

tableCell :: a -> Column a -> String
tableCell a col = do
    padLeft (col.size) $ col.value a

tableRow :: [Column a] -> a -> String
tableRow cols a = do
    stringRow $ map (tableCell a) cols


headerRow :: [Column a] -> String
headerRow cols =
    stringRow $ map (\c -> padLeft c.size c.label) cols

stringRow :: [String] -> String
stringRow cells =
    "|" <> (List.intercalate " |" cells) <> " |"

debug :: Show a => String -> a -> a
debug msg a = trace (msg <> ": " <> show a) a

dump :: Show s => String -> s -> a -> a
dump msg s a = trace (msg <> ": " <> show s) a