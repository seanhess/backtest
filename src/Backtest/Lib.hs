{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Lib where

import Backtest.Prelude
import Backtest.Types hiding (history)
import Backtest.History (loadReturns, samples, toHistories)
import Backtest.Simulation (simulation, Actions, rebalance, withdraw, bondsFirst, balances, yearsLeft, history)
import Backtest.Strategy (staticWithdrawal, rebalanceFixed, rebalance525Bands, rebalancePrime, rebalancePrimeNew)
import Backtest.Strategy.ABW (withdrawABW, withdrawABW', pmt, pmt', amortizedWithdrawal)
import Backtest.MSWR (rateResults, isFailure)
import Backtest.Aggregate (aggregateWithdrawals, yearSpread)
import Debug.Trace (trace)
import Data.List as List


run :: IO ()
run = do
    rs <- loadReturns
    let hs = toHistories rs
    -- mapM_ print hs

    runSimulation hs


-- |        1908 |   $63028.55 |  $693844.76 |  $260712.57 |       $0.00 |  $-63029.55 | $-156233.59 |    $8703.08 |  $537612.17 |  $206387.10 |   CAPE 11.9 |
    -- runMSWRs hs
    -- runAggregates hs

    pure ()





runSimulation :: [History] -> IO ()
runSimulation hs = do

    let yrs = 50
    let ss = samples yrs hs
    let start = thousand60

    -- let wda = staticWithdrawal swr4 start :: USD Amt Withdrawal
    -- let sim = simulation start $ do
    --             withdraw wda
    --             rebalance $ rebalancePrimeNew start.stocks

    let sim = simulation start $ do
                rebalance $ rebalancePrime start.stocks
                withdrawABW
    let srs = map sim ss :: [SimResult]

    -- print $ head hs


    printWithdrawalSpreadHeader

    -- * Show all years
    forM_ srs $ \sr -> do
        printSimResult sr
        printWithdrawalSpreadRow sr.wdSpread

    putStrLn ""
    let aws = aggregateWithdrawals $ map (.wdSpread) srs
    printAggregateWithdrawals aws

    putStrLn ""
    printWithdrawalSpread $ yearSpread srs

        -- mapM_ print $ sr.years

    putStrLn ""

    -- * Count failures
    -- print $ (length $ filter isFailure srs, length srs)
    -- print $ successRate srs
    -- mapM_ printSimResult $ filter isFailure srs

    -- print $ averagePortfolio srs
    -- print $ medianPortfolio srs

    -- * 1903 failure year
    (Just s1903) <- pure $ List.find (isYear 1903) srs
    print $ s1903.startYear
    print $ s1903.endBalance

    printYearHeader
    mapM_ printYear $ s1903.years

    -- * 1966 failure year
    -- (Just s1966) <- pure $ List.find (isYear 1966) srs
    -- print $ s1966.startYear
    -- print $ s1966.endBalance
    -- mapM_ (putStrLn . showYear) $ s1966.years
    -- print $ isFailure s1966
    pure ()

runAggregates :: [History] -> IO ()
runAggregates hs = do
    let years = 50
    let ss = samples years hs
    let ps = pct 60
    let bal = thousand ps


    putStrLn "Rebalance Fixed"
    putStrLn "----------------"
    runAggregate ss bal $ do
        withdrawABW
        rebalance $ rebalanceFixed ps
    putStrLn ""

    putStrLn "Prime Harvesting"
    putStrLn "----------------"
    runAggregate ss bal $ do
        withdrawABW
        rebalance $ rebalancePrime bal.stocks
    putStrLn ""

    putStrLn "Prime Harvesting New"
    putStrLn "----------------"
    runAggregate ss bal $ do
        withdrawABW
        rebalance $ rebalancePrimeNew bal.stocks
    putStrLn ""


runAggregate :: [[History]] -> Balances -> Actions () -> IO ()
runAggregate ss start acts = do 
    let sim = simulation start acts
    let srs = map sim ss :: [SimResult]
    let aws = aggregateWithdrawals $ map (.wdSpread) srs :: AggregateWithdrawals


    printAggregateWithdrawals aws

    -- printWithdrawalSpreadRow aws.totalSpread
    -- printWithdrawalSpreadRow aws.worstSpread
    -- printWithdrawalSpreadRow aws.numSamples
    -- forM_ [aws.totalSpread, aws.worstSpread, aws.numSamples] $ \(ws :: WithdrawalSpread) -> do





runMSWRs :: [History] -> IO ()
runMSWRs hs = do

   -- COMPARE MSWR
    -----------------

    -- oh it's doing better with years > 50 because there are fewer samples
    --  missing the 60s
    let years = 50

    let ss = samples years hs
    let ps = pct 60
    let bal = thousand ps


   
    putStrLn "Compare MSWRs"
    putStrLn "=============="
    putStrLn $ "Years: " <> show years
    putStrLn ""

    putStrLn "Rebalance Fixed"
    putStrLn "----------------"
    runMSWR ss bal (rebalance (rebalanceFixed ps))
    putStrLn ""

    putStrLn "Swedroe 5/25 bands"
    putStrLn "------------------"
    runMSWR ss bal (rebalance (rebalance525Bands ps))
    putStrLn ""

    putStrLn "Prime Harvesting"
    putStrLn "----------------"
    runMSWR ss bal (rebalance (rebalancePrime bal.stocks))
    putStrLn ""

    putStrLn "Prime Harvesting 2"
    putStrLn "------------------"
    runMSWR ss bal (rebalance (rebalancePrimeNew bal.stocks))
    putStrLn ""



runMSWR :: [[History]] -> Balances -> Actions () -> IO ()
runMSWR ss start reb = do
    let rrs = rateResults ss start reb allRates

    mapM_ printRateResult rrs
    pure ()

    where

        allRates :: [Pct Withdrawal]
        allRates = map pct [3.4, 3.5 .. 4.5]

        printRateResult :: RateResult -> IO ()
        printRateResult rr = do
            print ("RateResult", rr.rate, rr.success, millions rr.medEndPortfolio)

            -- mapM_ printSimResult $ head $ drop 10 srs

            -- print $ (length $ filter isFailure srs, length srs)
            -- print $ successRate srs
            -- mapM_ printSimResult $ filter isFailure srs

            -- let srs = rr.results :: [SimResult]
            -- (Just s1966) <- pure $ List.find (isYear 1966) srs
            -- print $ s1966.startYear
            -- print $ s1966.endBalance
            -- mapM_ (putStrLn . showYear) $ s1966.years
            -- print $ isFailure s1966

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
    putStrLn $ "|" <> (intercalate " |" $ map (padLeft p) items) <> " |"


padLeft :: Int -> String -> String
padLeft n s
    | length s < n = padLeft n (' ':s)
    | otherwise = s


isYear :: Int -> SimResult -> Bool
isYear y sr =
    sr.startYear == Year y

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

port :: Portfolio f -> (USD f Stocks, USD f Bonds)
port p = (p.stocks, p.bonds)

printSimResult :: SimResult -> IO ()
printSimResult sr = do
    print ("SimResult", sr.startYear, sr.endYear, sr.endBalance)


withdraw4 :: Balances -> Actions ()
withdraw4 start = do
    let const4Percent = loss $ staticWithdrawal swr4 start :: USD Amt Withdrawal
    withdraw const4Percent


thousand :: Pct Stocks -> Balances
thousand ps = rebalanceFixed ps $ Portfolio
  { stocks = usd $ 500
  , bonds = usd $ 500
  }

thousand50 :: Balances
thousand50 = Portfolio (usd 500) (usd 500)

thousand60 :: Balances
thousand60 = Portfolio (usd 600) (usd 400)

thousand70 :: Balances
thousand70 = Portfolio (usd 700) (usd 300)

thousand80 :: Balances
thousand80 = Portfolio (usd 800) (usd 200)

thousand90 :: Balances
thousand90 = Portfolio (usd 900) (usd 100)

swr4 :: Pct Withdrawal
swr4 = pct 4

rebalancePct :: Pct Stocks -> Actions ()
rebalancePct ps = rebalance $ rebalanceFixed ps

betweenYears :: Year -> Year -> History -> Bool
betweenYears start end h = 
    start <= h.year && h.year <= end


