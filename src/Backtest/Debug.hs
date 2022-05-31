module Backtest.Debug where

import Backtest.Types
import Backtest.Prelude
import Data.List as List


printYears :: [YearStart] -> IO ()
printYears ys = do
  printYearHeader
  mapM_ printYear ys

printYearHeader :: IO ()
printYearHeader =
    printTableRow 9
      [ "Year"
      , "beg.stck"
      , "beg.bnds"
      , "ret.stck"
      , "ret.bnds"
      , "Withdrawal"
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
      , show yr.start.stocks
      , show yr.start.bonds
      , show yr.returns.stocks
      , show yr.returns.bonds
      , show yr.withdrawal
      , show yr.actions.stocks
      , show yr.actions.bonds
      , show yr.end.stocks
      , show yr.end.bonds
      , fromMaybe "" $ (show . (.cape)) <$> yr.history
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

printAggregateWithdrawals :: AggregateWithdrawals -> IO ()
printAggregateWithdrawals aw = do

    printWithdrawalSpreadHeader

    putStrLn "\nTotals:"
    printWithdrawalSpreadRow aw.totalSpread

    putStrLn "\nWorst Sample:"
    printWithdrawalSpreadRow aw.worstSpread

    putStrLn "\nNum Samples:"
    printWithdrawalSpreadRow aw.numSamples

printWithdrawalSpreadHeader :: IO ()
printWithdrawalSpreadHeader = do
    printTableRow 9 ["low%", "2.0%", "2.5%", "3.0%", "3.5%", "4.0%", "4.5%", "high%"]

printWithdrawalSpreadRow :: Show a => WithdrawalSpread a -> IO ()
printWithdrawalSpreadRow ws = do
    printTableRow 9 [show ws.wlow, show ws.w2_0, show ws.w2_5, show ws.w3_0, show ws.w3_5, show ws.w4_0, show ws.w4_5, show ws.whigh]

printTableRow :: Int -> [String] -> IO ()
printTableRow p items = do
    putStrLn $ "|" <> (List.intercalate " |" $ map (padLeft p) items) <> " |"


padLeft :: Int -> String -> String
padLeft n s
    | length s < n = padLeft n (' ':s)
    | otherwise = s

