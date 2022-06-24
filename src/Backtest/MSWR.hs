module Backtest.MSWR where


import Backtest.Prelude
import Backtest.Types
import Backtest.Simulation
import Backtest.Strategy
import Backtest.Aggregate
import Data.List as List
import Data.List.NonEmpty as NE (filter)

rateResults :: NonEmpty (NonEmpty History) -> Balances -> Actions () -> [Pct Withdrawal] -> [RateResult]
rateResults ss start reb rates =
    map (runRateResult ss start reb) rates

toRateResult :: Pct Withdrawal -> NonEmpty SimResult -> RateResult
toRateResult wdp srs =
  RateResult
    { rate = wdp
    , success = successRate srs
    , results = srs
    , avgEndPortfolio = averageEndPortfolio srs
    , medEndPortfolio = medianEndPortfolio srs
    }
        
runRateResult :: NonEmpty (NonEmpty History) -> Balances -> Actions () -> Pct Withdrawal -> RateResult
runRateResult ss start reb wdp = toRateResult wdp $ runRate ss start reb wdp

runRate :: NonEmpty (NonEmpty History) -> Balances -> Actions () -> Pct Withdrawal -> NonEmpty SimResult
runRate ss start reb wdp =
    let wda = loss $ staticWithdrawal wdp start :: USD (Amt Withdrawal)
    in fmap (runSimulation wda) ss

  where
    runSimulation :: USD (Amt Withdrawal) -> NonEmpty History -> SimResult
    runSimulation wda =
        simulation start $ do
            withdraw (loss wda)
            reb








successRate :: NonEmpty SimResult -> Pct Success
successRate srs =
    let n = length $ NE.filter isFailure srs
        p = pctFromFloat $ 1 - (fromIntegral n / fromIntegral (length srs))
    in p

isFailure :: SimResult -> Bool 
isFailure sr =
    sr.endBalance == Portfolio mempty mempty

-- maximumSuccessRate :: Pct Success
-- maximumSuccessRate = pct 99.0 

-- isSuccessful :: Pct Success -> Bool
-- isSuccessful p = p >= maximumSuccessRate