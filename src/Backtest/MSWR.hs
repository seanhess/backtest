module Backtest.MSWR where


import Backtest.Prelude
import Backtest.Types
import Backtest.Simulation
import Backtest.Strategy
import Data.List as List

rateResults :: [[History]] -> Balances -> Actions () -> [Pct Withdrawal] -> [RateResult]
rateResults ss start reb rates =
    map runRate rates

  where

    runRate :: Pct Withdrawal -> RateResult
    runRate wdp =
        let wda = loss $ staticWithdrawal wdp start :: USD Amt Withdrawal
            srs = map (runSim wda) ss
        in RateResult
            { rate = wdp
            , success = successRate srs
            , results = srs
            , avgEndPortfolio = averageEndPortfolio srs
            , medEndPortfolio = medianEndPortfolio srs
            }

    runSim :: USD Amt Withdrawal -> [History] -> SimResult
    runSim wda =
        simulation start $ do
            withdraw (loss wda)
            reb

-- mswr :: [RateResult] -> Maybe RateResult
-- mswr rrs =
--     List.find (isSuccessful . (.success)) $ reverse rrs








successRate :: [SimResult] -> Pct Success
successRate srs =
    let n = length $ filter isFailure srs
        p = pctFromFloat $ 1 - (fromIntegral n / fromIntegral (length srs))
    in p

isFailure :: SimResult -> Bool 
isFailure sr =
    sr.endBalance == Portfolio mempty mempty

-- maximumSuccessRate :: Pct Success
-- maximumSuccessRate = pct 99.0 

-- isSuccessful :: Pct Success -> Bool
-- isSuccessful p = p >= maximumSuccessRate