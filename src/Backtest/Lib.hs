{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Backtest.Lib where

import Backtest.Prelude
import Backtest.Types hiding (history)
import Backtest.History
import Backtest.Simulation (simulation, simulation', Actions, rebalance, withdraw, bondsFirst, balances, yearsLeft, now, income, expense, yearsElapsed, onYears, withdrawals)
import Backtest.Strategy
import Backtest.Strategy.ABW
import Backtest.Strategy.Steps
import Backtest.Strategy.Peak
import Backtest.Graph
import Backtest.MSWR (rateResults, isFailure)
import Backtest.Aggregate
import Backtest.Optimize
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


    -- mapM_ print hs

    -- runSimulation 60 (pct 75) hs
    -- runMSWRs 60 (pct 60) hs
    -- runCrashes 50 hs
    runActual hs
    -- runAggregates 60 (pct 75) hs
    -- runChart 60 hs

    pure ()



-- 3.53% of peak!

runActual :: NonEmpty History -> IO ()
runActual hs = do
    countHistories
    -- findBest


    -- at 95% stocks I can support 40k/1275k withdrawals and 3.1% raise
    -- mapped to the peak that's 3.1% of 1500 = $46.5k
    -- but I have to rebalance to 95%!
    -- plus I have international which should help a lot!

    --           |    init |     low |     p10 |     p25 |     med |     p75 |     p90 |
    -- |  3.100% |  $40.00 |  $40.00 |  $46.00 |  $46.00 |  $79.16 | $119.96 | $166.56 |

    -- |  3.170% |  $40.42 |  $40.42 |  $45.93 |  $45.93 |  $75.40 | $114.77 | $155.96 |

    -- 95% |  3.100% |  $40.00 |  $40.00 |  $44.59 |  $44.59 |  $69.47 | $113.64 | $160.45 |
    -- of peak: 1477 - 51.5 = 1425 * 3.17% = $45.17 - YIKES!
    -- 10% stocks = 127.5k
    

    -- oh, generate all the input permutations first
    -- let inputs = permutations3 allRaises allAllocs allStarts

  

    -- * 1966 failure year

    -- let ps = pct 90 :: Pct Stocks
    -- let swr = pct 3.17 :: Pct Withdrawal
    -- let sw = amount swr (total start)
    -- let raise = pct 110 :: Pct Raise
    -- let sim = simulation' start $ actions sw start ps swr raise
    -- let srs = fmap sim ss :: NonEmpty SimResult

    -- printWithdrawalResultsByYear $ aggregateResults srs

    -- (Just s1966) <- pure $ List.find (isYear 1966) srs
    -- print $ s1966.startYear
    -- print $ s1966.endBalance
    -- printYearHeader
    -- mapM_ printYear $ s1966.years
    -- print $ isFailure s1966

    -- toChartFile  "graphs/withdrawals.html"
    --   [ withdrawalBinChart 60 (simData srs)
    --   , withdrawalLineChart $ simData srs
    --   , withdrawalStackChart $ medianWithdrawals srs 
    --   ]


    pure ()

  where

    ss = samples 20 hs
    bnds = usd 411.8 :: USD (Bal Bonds)
    stks = usd 1065.5
    kids = usd 161.3
    start = Portfolio (usd 1275) (usd 0)

    allRaises = map pct [3.0, 3.1 .. 3.5] :: [Pct Withdrawal]
    allAllocs = map pct [85, 90 .. 100] :: [Pct Stocks]
    allStarts = map usd [1] :: [USD (Amt Withdrawal)]

    countHistories :: IO ()
    countHistories = do
      forM_ ss $ \s -> do
        print $ NE.toList $ fmap (.year) s

    findBest :: IO ()
    findBest = do

      let isValid = const True -- \wd -> wd.low > (usd 30)
      let res = runAll3 allAllocs allRaises allStarts isValid runSearch :: [(Pct Stocks, Pct Withdrawal, USD (Amt Withdrawal), NonEmpty SimResult)]

      forM_ res $ \(ps, swr, _, srs) -> do
        -- print (ps, swr)
        -- print $ median $ fmap ((.low) . (.wdAmts)) srs
        -- forM_ srs $ \sr -> do
        --    putStrLn $ tableRow (withdrawalResultsCols) sr.wdAmts
        -- putStrLn $ tableRow (columns ps swr) (map (l.length) $ aggregateResults srs)
        print (ps, swr, sum $ fmap (head . (fmap (.withdrawal)) . (.years)) srs)

      pure ()


    runSearch :: Pct Stocks -> Pct Withdrawal -> USD (Amt Withdrawal) -> NonEmpty SimResult
    runSearch ps rs sw = 
      let acts = actions sw start ps rs
          sim = simulation' start acts
          srs = fmap sim ss :: NonEmpty SimResult
      in srs -- $ aggregateResultsAll $ aggregateResults $ srs
      

    -- runRaise :: NonEmpty (NonEmpty History) -> Pct Stocks -> Balances -> USD (Amt Withdrawal) -> Pct Withdrawal -> Pct Raise -> IO ()
    -- runRaise ss ps start sw swr raise = do
    --     -- print ("stocks:", ps, "init:", sw, "swr:", swr, "raise:", raise, "start:")

    --     let acts = actions sw start ps swr
    --     let sim = simulation' start acts
    --     let srs = fmap sim ss :: NonEmpty SimResult


        -- let cols = 
        -- printWithdrawalResultsByYear $ aggregateResults srs
        -- printWithdrawalResultsRow (show swr) 
        -- putStrLn $ tableRow (columns ps swr) $ aggregateResultsAll $ aggregateResults srs


    columns :: Pct Stocks -> Pct Withdrawal -> [Column WithdrawalResults]
    columns ps swr =
        [ Column "stocks%" 8 (\_ -> show ps)
        , Column "swr" 7 (\_ -> show swr)
        ] <> withdrawalResultsCols


    actions :: USD (Amt Withdrawal) -> Balances -> Pct Stocks -> Pct Withdrawal -> History -> Actions ()
    actions sw start' ps swr startH =
        -- TODO is peak lower than the original??
        let wd = peakWithdrawal (reverseTimeline startH.year hs) swr start'
        in do

        n <- now
        -- withdrawRaised sw swr raise
        withdrawFloor wd swr
        -- withdraw sw

        -- TODO this isn't adjusted for inflation
        -- onYears [0..2] $ do
        --     expense $ usd $ 9.6 + 13.2

        -- onYears [3..4] $ do
        --     expense $ usd $ 6.4 + 13.2

        -- onYears [5..7] $ do
        --     expense $ usd $ 3.4 + 13.2

        -- onYears [30..60] $ do
        --     income $ usd 25

        rebalance $ rebalanceFixed ps

    




runSimulation :: YearsLeft -> Pct Stocks -> NonEmpty History -> IO ()
runSimulation _ _ hs = do

    let yrs = 30
    let ps = pct 75
    let ss = samples yrs hs
    let start = thousand ps


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

                rebalance $ rebalanceFixed ps
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
    printYearHeader
    mapM_ printYear $ s1966.years
    print $ isFailure s1966
    pure ()


    -- toExampleChart $ withdrawalBinChart $ simData srs
    toChartFile  "graphs/withdrawals.html"
      [ withdrawalBinChart yrs (simData srs)
      , withdrawalLineChart $ simData srs
      , withdrawalStackChart $ medianWithdrawals srs
      ]

    pure ()



runAggregates :: YearsLeft -> Pct Stocks -> NonEmpty History -> IO ()
runAggregates years ps hs = do
    let ss = samples years hs
    let bal = thousand ps

    putStrLn "Rebalance Fixed"
    putStrLn "----------------"
    runAggregate ss bal $ const $ do
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
    runAggregate ss bal $ const $ do
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
    runAggregate ss bal $ \h ->
        let swr = pct 3.31
            wd = peakWithdrawal (reverseTimeline h.year hs) swr bal
        in do n <- now
              withdrawFloor (usd 10) (pct 3.31)
              rebalance $ rebalanceFixed ps
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


runMSWRs :: YearsLeft -> Pct Stocks -> NonEmpty History -> IO ()
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


