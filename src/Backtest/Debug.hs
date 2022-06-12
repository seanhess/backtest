module Backtest.Debug where

import Backtest.Types
import Backtest.Prelude
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
      [ fromMaybe "" $ (show . (.year)) <$> yr.history
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
      , fromMaybe "" $ (show . fromCAPE . (.cape)) <$> yr.history
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

printAggregateSpread :: AggregateSpread -> IO ()
printAggregateSpread aw = do

    printWithdrawalSpreadHeader

    printWithdrawalSpreadRow "Total" aw.totalSpread
    printWithdrawalSpreadRow "Worst" aw.worstSpread
    printWithdrawalSpreadRow "Num" aw.numSamples

printWithdrawalSpreadHeader :: IO ()
printWithdrawalSpreadHeader = do
    printTableRow 9 ["", "low%", "2.0%", "2.5%", "3.0%", "3.5%", "4.0%", "4.5%", "5.0%", "5.5%", "high%"]

printWithdrawalSpreadRow :: Show a => String -> WithdrawalSpread a -> IO ()
printWithdrawalSpreadRow lbl ws = do
    printTableRow 9 [lbl, show ws.wlow, show ws.w2_0, show ws.w2_5, show ws.w3_0, show ws.w3_5, show ws.w4_0, show ws.w4_5, show ws.w5_0, show ws.w5_5, show ws.whigh]

printWithdrawalResultsHeader :: IO ()
printWithdrawalResultsHeader = do
    printTableRow 8 ["", "init", "low", "p10", "p25", "med", "p75", "p90"]

printWithdrawalResultsRow :: String -> WithdrawalResults -> IO ()
printWithdrawalResultsRow lbl wr = do
    printTableRow 8 [lbl, show wr.init, show wr.low, show wr.p10, show wr.p25, show wr.med, show wr.p75, show wr.p90]

printWithdrawalResultsByYear :: NonEmpty (WithdrawalResults) -> IO ()
printWithdrawalResultsByYear wrs = do
    printWithdrawalResultsHeader
    forM_ (zip [1..] (NE.toList wrs)) $ \(y, wr) -> do
        printWithdrawalResultsRow (show y) wr

printTableRow :: Int -> [String] -> IO ()
printTableRow p items = do
    putStrLn $ "|" <> (List.intercalate " |" $ map (padLeft p) items) <> " |"


withdrawalResultsCols :: [Column WithdrawalResults]
withdrawalResultsCols =
    [ Column "init" 8 $ show . (.init)
    , Column "low" 8  $ show . (.low)
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

    