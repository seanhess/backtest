{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Lib where

import Backtest.Prelude
import Backtest.Types hiding (history)
import Backtest.History (loadReturns, samples, toHistories)
import Backtest.Simulation (simulation, Actions, rebalance, withdraw, bondsFirst, balances, yearsLeft, history)
import Backtest.Strategy (staticWithdrawal, rebalanceFixed, rebalance525Bands, rebalancePrime, rebalancePrimeNew)
import Backtest.Strategy.ABW (withdrawABW)
import Backtest.MSWR (rateResults, isFailure)
import Debug.Trace (trace)
import Data.List as List


run :: IO ()
run = do
    rs <- loadReturns
    let hs = toHistories rs
    -- mapM_ print hs

    -- runMSWRs hs
    runSample hs

    pure ()





runSample :: [History] -> IO ()
runSample hs = do

    -- RUN ONE SAMPLE
    --------------------
    let yrs = 30
    let ss = samples yrs hs
    let ps = (pct 60)
    let start = million ps
    -- let wda = staticWithdrawal swr4 start :: USD Amt Withdrawal
    -- let sim = simulation start $ do
    --             action $ withdrawBondsFirst wda
    --             action $ rebalancePrimeNew start.stocks
    -- how many years are left?
    let sim = simulation start $ do
                withdrawABW
                rebalance $ rebalanceFixed ps
    let srs = map sim ss :: [SimResult]

    -- let ys = (head srs).years
    -- mapM_ print $ take 10 ys

    -- * Show all years
    forM_ srs $ \sr -> do
        printSimResult sr
        putStrLn $ " init: " <> show sr.withdrawals.init

        putStrLn $ "  d50: " <> show sr.withdrawals.drawdown50
        putStrLn $ "  d40: " <> show sr.withdrawals.drawdown40
        putStrLn $ "  d30: " <> show sr.withdrawals.drawdown30
        putStrLn $ "  d20: " <> show sr.withdrawals.drawdown20
        putStrLn $ "  d10: " <> show sr.withdrawals.drawdown10

        putStrLn $ "  low: " <> show sr.withdrawals.low
        putStrLn $ "  p10: " <> show sr.withdrawals.p10
        putStrLn $ "  p25: " <> show sr.withdrawals.p25
        putStrLn $ "  med: " <> show sr.withdrawals.med
        putStrLn $ "  p75: " <> show sr.withdrawals.p75
        putStrLn $ "  p90: " <> show sr.withdrawals.p90


    -- * Count failures
    -- print $ (length $ filter isFailure srs, length srs)
    -- print $ successRate srs
    -- mapM_ printSimResult $ filter isFailure srs

    -- print $ averagePortfolio srs
    -- print $ medianPortfolio srs

    -- * 1966 failure year
    -- (Just s1966) <- pure $ List.find (isYear 1966) srs
    -- print $ s1966.startYear
    -- print $ s1966.endBalance
    -- mapM_ (putStrLn . showYear) $ s1966.years
    -- print $ isFailure s1966
    pure ()

runMSWRs :: [History] -> IO ()
runMSWRs hs = do

   -- COMPARE MSWR
    -----------------

    -- oh it's doing better with years > 50 because there are fewer samples
    --  missing the 60s
    let years = 50

    let ss = samples years hs
    let ps = pct 60
    let bal = million ps


   
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



isYear :: Int -> SimResult -> Bool
isYear y sr =
    sr.startYear == Year y

showYear :: YearResult -> String
showYear yr =
    show ("YearResult", yr.history.year, yr.withdrawal, yr.start, yr.end)

printSimResult :: SimResult -> IO ()
printSimResult sr = do
    print ("SimResult", sr.startYear, isFailure sr, sr.endYear, sr.endBalance)


withdraw4 :: Balances -> Actions ()
withdraw4 start = do
    let const4Percent = loss $ staticWithdrawal swr4 start :: USD Amt Withdrawal
    withdraw const4Percent


million :: Pct Stocks -> Balances
million ps = rebalanceFixed ps $ Portfolio
  { stocks = usd $ 500*1000
  , bonds = usd $ 500*1000
  }

million50 :: Balances
million50 = million (pct 50)

million60 :: Balances
million60 = million (pct 60)

million70 :: Balances
million70 = million (pct 70)

million80 :: Balances
million80 = million (pct 80)

million90 :: Balances
million90 = million (pct 90)

swr4 :: Pct Withdrawal
swr4 = pct 4

rebalancePct :: Pct Stocks -> Actions ()
rebalancePct ps = rebalance $ rebalanceFixed ps

betweenYears :: Year -> Year -> History -> Bool
betweenYears start end h = 
    start <= h.year && h.year <= end


