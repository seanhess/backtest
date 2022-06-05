{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Lib where

import Backtest.Prelude
import Backtest.Types hiding (history)
import Backtest.History
import Backtest.Simulation (simulation, Actions, rebalance, withdraw, bondsFirst, balances, yearsLeft, now, income)
import Backtest.Strategy
import Backtest.Strategy.ABW
import Backtest.Strategy.Steps
import Backtest.Strategy.Peak
import Backtest.Graph
import Backtest.MSWR (rateResults, isFailure)
import Backtest.Aggregate
import Debug.Trace (trace, traceM)
import Backtest.Debug
import Data.List as List

-- |     1999 |  $720.00 |  $167.45 |  $211.35 |   $17.35 |   $71.72 | $-212.35 |  $139.63 |  $720.00 |  $324.43 |CAPE 40.58 |

run :: IO ()
run = do
    rs <- loadReturns
    let hs = toHistories rs


    -- mapM_ print hs

    runSimulation 60 (pct 75) hs
    -- runMSWRs 60 (pct 60) hs
    -- runCrashes 50 hs
    -- runActual hs
    -- runAggregates 60 (pct 75) hs
    -- runChart 60 hs

    pure ()



-- 3.53% of peak!

runActual :: [History] -> IO ()
runActual hs = do
    let ps = pct 90
    let ss = samples 60 hs
    let bnds = usd 411.8 :: USD (Bal Bonds)
    let stks = usd 1065.5
    let kids = usd 161.3
    let start = Portfolio stks (bnds + (fromUSD $ loss kids))

    let allRaises = map pct [3.50, 3.51 .. 3.60] :: [Pct Withdrawal]
    let allAllocs = map pct [60, 65 .. 100] :: [Pct Stocks]
  
    -- forM_ allRaises $ \x -> do
    --     runRaise ss (pct 100) start x -- (pct 3.53)

    forM_ allAllocs $ \x -> do
        runRaise ss x start (pct 3.53)

    forM_ allAllocs $ \x -> do
        runRaise ss x start (pct 3.57)

    -- TODO I need a better instrument for this. A graph?
    -- no, I need a way to optimize. Median withdrawal? 10th percentile median withdrawal
    -- 3.5% is the winner for 75/25!
    -- 3.5 75/25 is 92 95 97 98
    -- 3.5 80/20 is 92 95 97 98
    -- 3.5 85/15 is 92 96 93 99

    -- 3.5 100/0 is 90 92 89
    -- 3.5 60/40 is 95 97 92
    -- 3.5 90/10 is 90 95 92

    -- 525 75/25 is 92 95 95 98

    -- ignoring rebalancing (prime-like) doesn't work quite as well (90s instead)
    -- I should have liabilities in bonds + my allocation

    -- that's nice!

    -- * 1966 failure year

    -- let sim = simulation start $ actions start ps (pct 3.5)
    -- let srs = map sim ss :: [SimResult]

    -- (Just s1966) <- pure $ List.find (isYear 1966) srs
    -- print $ s1966.startYear
    -- print $ s1966.endBalance
    -- printYearHeader
    -- mapM_ printYear $ s1966.years
    -- print $ isFailure s1966
    -- pure ()

  where

    runRaise :: [[History]] -> Pct Stocks -> Balances -> Pct Withdrawal -> IO ()
    runRaise ss ps start raise = do
        print ("stocks:", ps, "raise:", raise)
        runAggregate ss start $ actions start ps raise

    -- TODO, deduct something first?
    actions :: Balances -> Pct Stocks -> Pct Withdrawal -> Actions ()
    actions start ps raise = do

        bal <- balances
        yl <- yearsLeft

        -- let ks = kidExpenses yl
        -- let start' = adjustKids ks start

        let wda = staticWithdrawal raise start

        let ss = socialSecurity yl
        income ss


        -- let bal' = adjustKids ks bal

        -- let wf = withdrawalFloor wda raise bal
        -- withdraw wf

        n <- now
        withdrawPeak (historyPeak n.year hs) raise

        -- traceM $ show ("act", yl, wf, ss, ks)
        -- rebalance $ rebalance525Bands ps
        rebalance $ rebalanceFixed ps

    -- kidExpenses yl
    --   | yl > (60 - 5) = usd $ 9.6 + 13.2
    --   | yl > (60 - 7) = usd $ 6.4 + 13.2
    --   | yl > (60 - 10) = usd $ 3.4 + 13.2
    --   | otherwise = usd 0

    socialSecurity yl
      | yl < 30 = usd 25
      | otherwise = usd 0

    -- adjustKids :: USD (Amt Bonds) -> Balances -> Balances
    -- adjustKids ks bal =
    --     bal { bonds = bal.bonds - (fromUSD $ gain ks) }



    




runSimulation :: YearsLeft -> Pct Stocks -> [History] -> IO ()
runSimulation yrs ps hs = do

    let ss = samples yrs hs
    let start = thousand ps


    -- (Just h1966) <- pure $ List.find (\h -> h.year == Year 1966) hs
    -- print h1966
    -- let rets = returnsWithRecentHistory 20 thousand50 h1966 hs
    -- print $ rets
    -- print $ sum rets


    let p = pct 3.3

    let sim = simulation start $ do
                n <- now

                -- withdrawPeak (historyPeak n.year hs) p
                withdrawFloor (staticWithdrawal p start) p

                rebalance $ rebalanceFixed ps
                -- rebalance $ rebalancePrimeNew start.stocks
    let srs = map sim ss :: [SimResult]



    printWithdrawalResultsHeader

    forM_ srs $ \sr -> do
        printWithdrawalResultsRow (show sr.startYear) sr.wdAmts

    -- print $ head hs


    -- printWithdrawalSpreadHeader

    -- -- * Show all years
    -- forM_ srs $ \sr -> do
    --     printSimResult sr
    --     printWithdrawalSpreadRow sr.wdSpread

    -- putStrLn ""
    -- let aws = aggregateWithdrawals $ map (.wdSpread) srs
    -- printAggregateWithdrawals aws

    -- putStrLn ""
    -- printWithdrawalSpread $ yearSpread srs


    -- * Count failures
    -- print $ (length $ filter isFailure srs, length srs)
    -- print $ successRate srs
    -- mapM_ printSimResult $ filter isFailure srs

    -- print $ averagePortfolio srs
    -- print $ medianPortfolio srs



    -- * 1966 failure year
    (Just s1966) <- pure $ List.find (isYear 1982) srs
    print $ s1966.startYear
    print $ s1966.endBalance
    printYearHeader
    mapM_ printYear $ s1966.years
    print $ isFailure s1966
    pure ()


    -- toExampleChart $ withdrawalBinChart $ simData srs
    toChartFile  "graphs/withdrawal-bin.html" $ withdrawalBinChart $ simData srs
    toChartFile  "graphs/withdrawal-line.html" $ withdrawalLineChart $ simData srs

    pure ()



runAggregates :: YearsLeft -> Pct Stocks -> [History] -> IO ()
runAggregates years ps hs = do
    let ss = samples years hs
    let bal = thousand ps

    putStrLn "Rebalance Fixed"
    putStrLn "----------------"
    runAggregate ss bal $ do
        withdrawABW
        rebalance $ rebalanceFixed ps
    putStrLn ""

    -- putStrLn "Fixed Dips"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawABWDips
    --     rebalance $ rebalanceFixed ps
    -- putStrLn ""

    -- putStrLn "Fixed Steps"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawSteps $ staticWithdrawal (pct 4) bal
    --     rebalance $ rebalanceFixed ps
    -- putStrLn ""

    -- putStrLn "Swedroe 5/25"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawABW
    --     rebalance (rebalance525Bands ps)

    -- putStrLn "Swedroe 5/25 Dips"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawABWDips
    --     rebalance (rebalance525Bands ps)

    -- putStrLn "Prime Harvesting ABW"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawABW
    --     rebalance $ rebalancePrime bal.stocks
    -- putStrLn ""

    -- putStrLn "Prime Harvesting New"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawABW
    --     rebalance $ rebalancePrimeNew bal.stocks
    -- putStrLn ""


    putStrLn "Floor Fixed 3.31"
    putStrLn "----------------"
    let swr100 = pct 3.31
    let wda = staticWithdrawal swr100 bal
    runAggregate ss bal $ do
        withdrawFloor wda swr100
        rebalance $ rebalanceFixed ps
    putStrLn ""

    -- putStrLn "Floor Fixed 3.3 60/40"
    -- putStrLn "----------------"
    -- -- let swr100 = pct 3.3
    -- -- let wda = staticWithdrawal swr100 bal
    -- runAggregate ss bal $ do
    --     withdrawFloor wda swr100
    --     rebalance $ rebalanceFixed (pct 60)
    -- putStrLn ""

    -- putStrLn "Floor Fixed 3.3 90/10"
    -- putStrLn "----------------"
    -- -- let swr100 = pct 3.3
    -- -- let wda = staticWithdrawal swr100 bal
    -- runAggregate ss bal $ do
    --     withdrawFloor wda swr100
    --     rebalance $ rebalanceFixed (pct 90)
    -- putStrLn ""

    -- putStrLn "Floor 525 3.3"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawFloor wda swr100
    --     rebalance $ rebalance525Bands ps
    -- putStrLn ""

    -- putStrLn "Floor No Rebalance"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawFloor (staticWithdrawal (pct 3.3) bal) swr100
    -- putStrLn ""

    -- putStrLn "Floor 525 70/30 3.3"
    -- putStrLn "----------------"
    -- -- let wda' = staticWithdrawal (pct 3.4) bal
    -- runAggregate ss (thousand (pct 70)) $ do
    --     withdrawFloor (staticWithdrawal (pct 3.3) bal) (pct 3.3)
    --     rebalance $ rebalance525Bands (pct 70)
    -- putStrLn ""

    putStrLn "Peak Fixed 3.31"
    putStrLn "----------------"
    runAggregate ss bal $ do
        n <- now
        withdrawPeak (historyPeak n.year hs) (pct 3.31)
        rebalance $ rebalanceFixed ps
    putStrLn ""





runAggregate :: [[History]] -> Balances -> Actions () -> IO ()
runAggregate ss start acts = do 
    let sim = simulation start acts
    let srs = map sim ss :: [SimResult]
    let wds = map (.wdSpread) srs

    printAggregateSpread $ aggregateSpread wds


    putStrLn "Median Results"
    printWithdrawalResultsHeader
    printWithdrawalResultsRow "10%" $ aggregatePercentile 0.10 $ map (.wdAmts) srs
    printWithdrawalResultsRow "25%" $ aggregatePercentile 0.25 $ map (.wdAmts) srs
    printWithdrawalResultsRow "med" $ aggregateMedian $ map (.wdAmts) srs
    printWithdrawalResultsRow "75%" $ aggregatePercentile 0.75 $ map (.wdAmts) srs
    printWithdrawalResultsRow "90%" $ aggregatePercentile 0.90 $ map (.wdAmts) srs

    putStrLn "Bad Years"
    print $ lowYears $ yearWds srs

    where
        yearWds :: [SimResult] -> [(Year, WithdrawalSpread Int)]
        yearWds srs =
            map (\sr -> (sr.startYear, sr.wdSpread)) srs

        isLow :: (Year, WithdrawalSpread Int) -> Bool
        isLow (_, s) = s.wlow > 0

        lowYears :: [(Year, WithdrawalSpread Int)] -> [Year]
        lowYears ys = map fst $ filter isLow ys



-- runCrashes :: YearsLeft -> [History] -> IO ()
-- runCrashes years hs = do
--     -- -- TODO remove samples with duration less than 3 years?
--     putStrLn $ intercalate ", " ["year", "cape", "depth", "length", "prior1y","prior2y","prior3y","prior4y","prior5y",     "years"]
--     forM_ (tails hs) $ \hs' -> do
--         start <- pure $ headMay hs'

--         let mc = flip (crashInfo hs) (crashes hs') =<< start :: Maybe Crash

--         case mc of
--             Nothing -> pure ()
--             Just (c :: Crash) -> do
--                 putStrLn $ intercalate ", "
--                   [ show c.start
--                   , show (fromCAPE c.cape)
--                   , show c.depth
--                   , show $ length c.years
--                   , show $ c.prior1y
--                   , show $ c.prior2y
--                   , show $ c.prior3y
--                   , show $ c.prior4y
--                   , show $ c.prior1y
--                   , show $ map (\h -> h.year) c.years
--                   ]
--                 -- putStrLn ""
--                 -- putStrLn $ "start: " <> show c.start
--                 -- putStrLn $ "cape: " <> show (fromCAPE c.cape)
--                 -- putStrLn $ "balance: " <> show (millions c.balance)
--                 -- putStrLn $ "depth: " <> show c.depth
--                 -- putStrLn $ "duration: " <> show (length c.years)
--                 -- mapM_ print c.years

--     -- mapM_ (print) $ map (map (map (.year)) . crashes) $ tails hs
--     pure ()


runMSWRs :: YearsLeft -> Pct Stocks -> [History] -> IO ()
runMSWRs years ps hs = do

   -- COMPARE MSWR
    -----------------

    -- oh it's doing better with years > 50 because there are fewer samples
    --  missing the 60s

    let ss = samples years hs
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
        allRates = map pct [3.2, 3.3 .. 4.5]

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



port :: Portfolio USD f -> (USD (f Stocks), USD (f Bonds))
port p = (p.stocks, p.bonds)

printSimResult :: SimResult -> IO ()
printSimResult sr = do
    print ("SimResult", sr.startYear, sr.endYear, sr.endBalance)



betweenYears :: Year -> Year -> History -> Bool
betweenYears start end h = 
    start <= h.year && h.year <= end


