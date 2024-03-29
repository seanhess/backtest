{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists #-}
module Backtest.Lib where

import Backtest.Prelude
import Backtest.Types hiding (history)
import Backtest.History
import Backtest.Simulation (simulation, simulation', Actions, rebalance, withdraw, bondsFirst, balances, yearsLeft, now, income, expense, yearsElapsed, onYears, withdrawals)
import Backtest.Strategy
import Backtest.Strategy.ABW
import Backtest.Strategy.Steps
import Backtest.Strategy.Peak
import Backtest.Graph.Vega
import Backtest.MSWR (rateResults, isFailure)
import Backtest.Aggregate
import Backtest.Optimize
import Backtest.Cache as Cache
import Debug.Trace (trace, traceM)
import Backtest.Debug
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Control.Monad

-- |     1999 |  $720.00 |  $167.45 |  $211.35 |   $17.35 |   $71.72 | $-212.35 |  $139.63 |  $720.00 |  $324.43 |CAPE 40.58 |

run :: IO ()
run = do
    rs <- loadReturns
    let hs = toHistories rs
    cache <- Cache.loadFromFile "data/cache.csv"
    -- dumpCache cache
    -- mapM_ print hs


    -- mapM_ print hs

    -- runSimulation 60 (pct 75) hs
    -- runMSWRs 60 (pct 60) hs
    -- runCrashes 50 hs
    runActual hs cache
    -- runAggregates 60 (pct 75) hs
    -- runChart 60 hs
    -- runCache hs

    pure ()


runCache :: NonEmpty History -> IO ()
runCache hs = do
  Right cache <- pure $ buildSWRCache $ calculateAllMSWRs hs
  saveToFile "data/cache.csv" cache

-- 3.53% of peak!

runActual :: NonEmpty History -> SWRCache -> IO ()
runActual hs cache = do
    -- countHistories
    -- findBest
    -- runSim
    -- tryOptimize
    tryMaximize
    pure ()

  where

    years = numYears 60

    ss = samples years hs

    -- total of kid expenses starting in 2023
    -- kids = usd 157.4
    start = Portfolio (usd 1230) (usd 50)
    -- start = Portfolio (usd 1000) (usd 0)



    expenses :: [Transaction Expense]
    expenses =
      [ Transaction "Child Support" (NumYears 0) 3 (usd 9.6)
      , Transaction "Child Support" (NumYears 3) 2 (usd 6.4)
      , Transaction "Child Support" (NumYears 5) 3 (usd 3.4)
      , Transaction "Extra Kids"    (NumYears 0) 8 (usd 13.2)
      ]


    saveGraphs :: NonEmpty SimResult -> IO ()
    saveGraphs srs =
      toChartFile  "graphs/withdrawals.html"
        [ withdrawalBinChart years (simData (usd 600) srs)
        , withdrawalLineChart $ simData (usd 600) srs
        , withdrawalStackChart $ medianWithdrawals srs
        ]

    runSim :: IO ()
    runSim = do

      -- this runs only ONE simulation?
      let al = S100
      let wr = pct 3.7
      let sim = simulation start (actions start al wr)
      let srs = fmap sim ss :: NonEmpty SimResult

      -- flip printTable (NE.toList srs) $
      --   [ Column "year" 9 (\sr -> show sr.startYear)
      --   , Column "min" 7 (\sr -> show $ minimum $ withdrawals sr)
      --   , Column "med" 7 (\sr -> show $ median $ withdrawals sr)
      --   ]
      
      saveGraphs srs

      (Just s1966) <- pure $ List.find (isYear 1966) srs
      print $ s1966.startYear
      print $ s1966.endBalance
      printTable yearCols $ NE.toList s1966.years


      print $ medWithdrawal srs
      pure ()



    tryMaximize :: IO ()
    tryMaximize = do

      let run' = \wr -> simulation start (actions start S100 wr) <$> ss

      let res = maximizeRate isSimValid run'
      printTable columns res
      -- print $ map (\(wr, srs) -> (wr, medWithdrawal srs)) res

    -- tryOptimize :: IO ()
    -- tryOptimize = do

    --   print $ length ss

    --   -- Optimize with history
    --   let res = optimize stepAlloc5 stepRate5 ss S100 (pct 2.8) start (actions start)
    --   -- let res = 

    --   printTable columns res

    --   -- Best result
    --   putStrLn "BEST"
    --   Just best <- pure $ bestResult res
    --   putStrLn $ tableRow columns best

    --   -- oh do I have to manually calculate this?
    --   let hn = History {year = Year 2023, returns = Portfolio (pct (-19.5)) (pct 0), values = Portfolio (usd (3066397.56 * 0.805)) (usd 49.86), cape = CAPE 36.94}

    --   let hx = hs <> [hn]
    --   putStrLn $ "Peak Withdrawal:"
    --   print $ peakWithdrawal (reverseTimeline hn.year hx) best.swr (rebalanceFixed best.alloc start)

    --   putStrLn $ "Peak Withdrawal (100% stocks, 3%):"
    --   print $ peakWithdrawal (reverseTimeline hn.year hx) (pct 3) (rebalanceFixed S100 start)

    --   pure ()

    actions :: Balances -> Allocation -> Pct Withdrawal -> Actions ()
    actions start' al wr = do

        -- get default starting withdrawal, if you can't find the old one
        -- sy <- startYear
        -- let swda = peakWithdrawal (reverseTimeline sy hs) wr start'

        -- TODO step up the withdrawal amount, not the rate
        let swda = staticWithdrawal wr start'

        n <- now
        -- withdrawCached cache al expenses swda
        withdrawFloor swda wr

        -- TODO this isn't adjusted for inflation
        onYears [0..2] $ do
            expense $ usd $ 9.6 + 13.2

        onYears [3..4] $ do
            expense $ usd $ 6.4 + 13.2

        onYears [5..7] $ do
            expense $ usd $ 3.4 + 13.2

        onYears [30..60] $ do
            income $ usd 25

        rebalance $ rebalanceFixed al

 
    columns :: [Column OptimizeResult]
    columns =
        -- [ Column "stocks%" 9 (\o -> show o.alloc)
        [ Column "swr" 7 (\o -> show o.swr)
        , Column "min" 7 (\o -> show $ minWithdrawal o.results)
        , Column "med" 7 (\o -> show $ medWithdrawal o.results)
        ]
   




runSimulation :: NumYears -> Pct Stocks -> NonEmpty History -> IO ()
runSimulation _ _ hs = do

    let yrs = NumYears 30
    let al = S75
    let ss = samples yrs hs
    let start = thousand al


    -- (Just h1966) <- pure $ List.find (\h -> h.year == Year 1966) hs
    -- print h1966
    -- let rets = returnsWithRecentHistory 20 thousand50 h1966 hs
    -- print $ rets
    -- print $ sum rets


    let p = pct 4.0

    let sim = simulation start $ do
                -- n <- now
                
                -- withdraw $ staticWithdrawal p start

                -- yl <- yearsLeft
                -- ye <- yearsElapsed
                -- expense $ usd $ fromIntegral ye
                onYears [0..9] $ do
                    expense $ usd 10

                onYears [20..30] $ do
                    income $ usd 10

                -- income $ usd 10
                -- expense $ usd 5
                -- expense $ usd 5
                -- withdrawPeak (historyPeak n.year hs) p
                withdrawFloor (staticWithdrawal p start) p

                rebalance $ rebalanceFixed al
                -- rebalance $ rebalancePrimeNew start.stocks
    let srs = fmap sim ss :: NonEmpty SimResult



    -- printWithdrawalResultsHeader

    -- forM_ srs $ \sr -> do
    --     printWithdrawalResultsRow (show sr.startYear) sr.wdAmts

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
    printTable yearCols $ NE.toList s1966.years
    print $ isFailure s1966
    pure ()




runAggregates :: NumYears -> Allocation -> NonEmpty History -> IO ()
runAggregates years al hs = do
    let ss = samples years hs
    let bal = thousand al

    putStrLn "Rebalance Fixed"
    putStrLn "----------------"
    runAggregate ss bal $ const $ do
        withdrawABW
        rebalance $ rebalanceFixed al
    putStrLn ""

    -- putStrLn "Fixed Dips"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawABWDips
    --     rebalance $ rebalanceFixed al
    -- putStrLn ""

    -- putStrLn "Fixed Steps"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawSteps $ staticWithdrawal (pct 4) bal
    --     rebalance $ rebalanceFixed al
    -- putStrLn ""

    -- putStrLn "Swedroe 5/25"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawABW
    --     rebalance (rebalance525Bands al)

    -- putStrLn "Swedroe 5/25 Dips"
    -- putStrLn "----------------"
    -- runAggregate ss bal $ do
    --     withdrawABWDips
    --     rebalance (rebalance525Bands al)

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
    runAggregate ss bal $ const $ do
        withdrawFloor wda swr100
        rebalance $ rebalanceFixed al
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
    --     rebalance $ rebalance525Bands al
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
    runAggregate ss bal $ \h ->
        let swr = pct 3.31
            wd = peakWithdrawal (reverseTimeline h.year hs) swr bal
        in do n <- now
              withdrawFloor (usd 10) (pct 3.31)
              rebalance $ rebalanceFixed al
    putStrLn ""





runAggregate :: NonEmpty (NonEmpty History) -> Balances -> (History -> Actions ()) -> IO ()
runAggregate ss start getActions = do 
    let sim = simulation' start getActions
    let srs = fmap sim ss :: NonEmpty SimResult
    let wds = fmap withdrawalSpread srs

    -- printAggregateSpread $ aggregateSpread wds


    -- putStrLn "Median Results"
    -- printWithdrawalResultsHeader
    -- printWithdrawalResultsRow "10%" $ aggregatePercentile 0.10 $ map (.wdAmts) srs
    -- printWithdrawalResultsRow "25%" $ aggregatePercentile 0.25 $ map (.wdAmts) srs
    -- printWithdrawalResultsRow "med" $ aggregateMedian $ map (.wdAmts) srs
    -- printWithdrawalResultsRow "75%" $ aggregatePercentile 0.75 $ map (.wdAmts) srs
    -- printWithdrawalResultsRow "90%" $ aggregatePercentile 0.90 $ map (.wdAmts) srs

    putStrLn "Bad Years"
    print $ lowYears $ yearWds srs

    where
        yearWds :: NonEmpty SimResult -> NonEmpty (Year, WithdrawalSpread Int)
        yearWds srs =
            fmap (\sr -> (sr.startYear, withdrawalSpread sr)) srs

        isLow :: (Year, WithdrawalSpread Int) -> Bool
        isLow (_, s) = s.wlow > 0

        lowYears :: NonEmpty (Year, WithdrawalSpread Int) -> [Year]
        lowYears ys = map fst $ NE.filter isLow ys



-- runCrashes :: NumYears -> [History] -> IO ()
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


runMSWRs :: NumYears -> Allocation -> NonEmpty History -> IO ()
runMSWRs years al hs = do

   -- COMPARE MSWR
    -----------------

    -- oh it's doing better with years > 50 because there are fewer samples
    --  missing the 60s

    let ss = samples years hs
    let bal = million al

    putStrLn "Compare MSWRs"
    putStrLn "=============="
    putStrLn $ "Years: " <> show years
    putStrLn ""

    putStrLn "Rebalance Fixed"
    putStrLn "----------------"
    runMSWR ss bal (rebalance (rebalanceFixed al))
    putStrLn ""

    putStrLn "Swedroe 5/25 bands"
    putStrLn "------------------"
    runMSWR ss bal (rebalance (rebalance525Bands al))
    putStrLn ""

    putStrLn "Prime Harvesting"
    putStrLn "----------------"
    runMSWR ss bal (rebalance (rebalancePrime bal.stocks))
    putStrLn ""

    putStrLn "Prime Harvesting 2"
    putStrLn "------------------"
    runMSWR ss bal (rebalance (rebalancePrimeNew bal.stocks))
    putStrLn ""



runMSWR :: NonEmpty (NonEmpty History) -> Balances -> Actions () -> IO ()
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


